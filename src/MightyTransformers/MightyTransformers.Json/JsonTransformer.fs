// ----------------------------------------------------------------------------------------------
// Copyright 2017 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

module MightyTransformers.Json.JsonTransformer

open System
open System.Globalization
open System.Text

open MiniJson.JsonModule
open MiniJson.DynamicJsonModule

[<RequireQualifiedAccess>]
type JContextElement =
  | Member  of  string
  | Index   of  int
  | Named   of  string
type JContext = JContextElement list

[<RequireQualifiedAccess>]
type JError =
  | CanNotConvertToFloat
  | CanNotConvertToString
  | IndexOutOfRange       of int
  | MemberNotFound        of string
  | Message               of string
  | NotAnArrayOrObject
  | NotABool
  | NotAFloat
  | NotAString
  | NotAnObject

[<RequireQualifiedAccess>]
type JErrorTree =
  | Empty
  | Leaf      of JContext*JError
  | Suppress  of JErrorTree
  | Fork      of JErrorTree*JErrorTree

type JResult<'T>(v : 'T, et : JErrorTree) =
  struct
    member x.Value      = v
    member x.ErrorTree  = et
  end

type JErrorItem =
  {
    IsSuppressed  : bool
    Path          : string
    Error         : JError
  }
  static member New i p e : JErrorItem = { IsSuppressed = i; Path = p; Error = e }

//

[<NoEquality>]
[<NoComparison>]
type JTransform<'T> (f : OptimizedClosures.FSharpFunc<Json, JContext, JResult<'T>>) = 
  struct
    member x.Invoke (j, p) = f.Invoke (j, p)
  end

module JTransform =
  open FSharp.Core.Printf

  module Details =

    let defaultSize     = 16

    let inline jtrans f = JTransform(OptimizedClosures.FSharpFunc<_, _, _>.Adapt f)

    let inline zero ()  = LanguagePrimitives.GenericZero<_>

    let inline leaf p e = JErrorTree.Leaf (p, e)

    let empty           = JErrorTree.Empty

    let inline isGood e=
      match e with
      | JErrorTree.Empty
      | JErrorTree.Suppress _ -> true
      | _                     -> false

    let inline supp e   =
      match e with
      | JErrorTree.Empty  -> e
      | _                 -> JErrorTree.Suppress e

    let inline join l r =
      match l, r with
      | JErrorTree.Empty      , _                     -> r
      | _                     , JErrorTree.Empty      -> l
      | JErrorTree.Suppress l , JErrorTree.Suppress r -> JErrorTree.Fork (l, r) |> JErrorTree.Suppress
      | _                     , _                     -> JErrorTree.Fork (l, r)

    let inline invoke (t : JTransform<'T>) j p = t.Invoke (j, p)

    let pathToString p =
      // Verified that call to private pathToString don't do "funny" stuff
      let rec pathToString (sb : StringBuilder) p =
        match p with
        | []    -> sb.Append "root" |> ignore
        | h::t  ->
          pathToString sb t
          match h with
          | JContextElement.Member n  -> sb.Append (sprintf ".%s"   n)  |> ignore
          | JContextElement.Index  i  -> sb.Append (sprintf ".[%d]" i)  |> ignore
          | JContextElement.Named  n  -> sb.Append (sprintf "(%s)"  n)  |> ignore
      let sb = System.Text.StringBuilder defaultSize
      pathToString sb p
      sb.ToString ()

    let collapse (et : JErrorTree) =
      // Verified that call to private collapse don't do "funny" stuff
      let rec collapse suppress (result : ResizeArray<_>) et =
        match et with
        | JErrorTree.Empty            -> ()
        | JErrorTree.Leaf     (p, e)  -> JErrorItem.New suppress (pathToString p) e |> result.Add
        | JErrorTree.Suppress e       -> collapse true result e
        | JErrorTree.Fork     (l, r)  -> collapse suppress result l; collapse suppress result r

      let result = ResizeArray defaultSize
      collapse false result et
      result.ToArray ()

    let inline result  v et = JResult (v, et)

    let inline good    v    = result v empty

    module Loops =
      let rec jmany t p (r : ResizeArray<_>) (ms : _ []) m et i =
        if i < ms.Length then
          let v = m ms.[i]
          let ip = (JContextElement.Index i)::p
          let tr = invoke t v ip
          if isGood tr.ErrorTree then
            r.Add tr.Value
          jmany t p r ms m (join et tr.ErrorTree) (i + 1)
        else
          et

      let rec jmember name dv t p (ms : _ []) i =
        if i < ms.Length then
          let k, v = ms.[i]
          if k = name then
            let ip = (JContextElement.Member name)::p
            invoke t v ip
          else
            jmember name dv t p ms (i + 1)
        else
          result dv (name |> JError.MemberNotFound |> leaf p)

  open Details

  // Monad

  let inline jreturn v : JTransform<'T> =
    jtrans <| fun j p ->
      good v

  let inline jbind (t : JTransform<'T>) (uf : 'T -> JTransform<'U>) : JTransform<'U> =
    jtrans <| fun j p ->
      let tr  = invoke t j p
      let u   = uf tr.Value
      let ur  = invoke u j p
      result ur.Value (join tr.ErrorTree ur.ErrorTree)

  // Kleisli

  let inline jarr f = fun v -> jreturn (f v)

  let inline jkleisli tf uf =
    fun v -> jbind (tf v) uf

  // Applicative

  let inline jpure v = jreturn v

  let inline japply (t : JTransform<'U -> 'V>) (u : JTransform<'U>) : JTransform<'V> =
    jtrans <| fun j p ->
      let tr  = invoke t j p
      let ur  = invoke u j p
      result (tr.Value ur.Value) (join tr.ErrorTree ur.ErrorTree)

  // Functor

  let inline jmap m (t : JTransform<'T>) : JTransform<'U> =
    jtrans <| fun j p ->
      let tr = invoke t j p
      result (m tr.Value) tr.ErrorTree

  // Combinators

  let inline jorElse (l : JTransform<'T>) (r : JTransform<'T>) : JTransform<'T> =
    jtrans <| fun j p ->
      let lr = invoke l j p
      if isGood lr.ErrorTree then
        lr
      else
        let rr = invoke r j p
        if isGood rr.ErrorTree then
          rr
        else
          result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline jkeepLeft (l : JTransform<'T>) (r : JTransform<_>) : JTransform<'T> =
    jtrans <| fun j p ->
      let lr = invoke l j p
      let rr = invoke r j p
      result lr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline jkeepRight (l : JTransform<_>) (r : JTransform<'T>) : JTransform<'T> =
    jtrans <| fun j p ->
      let lr = invoke l j p
      let rr = invoke r j p
      result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline jpair (l : JTransform<'T>) (r : JTransform<'U>) : JTransform<'T*'U> =
    jtrans <| fun j p ->
      let lr = invoke l j p
      let rr = invoke r j p
      result (lr.Value, rr.Value) (join lr.ErrorTree rr.ErrorTree)

  let inline jsuppress (t : JTransform<'T>) : JTransform<'T> =
    jtrans <| fun j p ->
      let tr = invoke t j p
      let e  =
        if isGood tr.ErrorTree then
          tr.ErrorTree
        else
          tr.ErrorTree |> supp
      result tr.Value e

  let inline jtoOption (t : JTransform<'T>) : JTransform<'T option> =
    jtrans <| fun j p ->
      let tr = invoke t j p
      if isGood tr.ErrorTree then
        good (Some tr.Value)
      else
        good None

#if FSHARP_41
  let inline jtoResult (t : JTransform<'T>) : JTransform<Result<'T, JErrorItem []>> =
    jtrans <| fun j p ->
      let tr = invoke t j p
      if isGood tr.ErrorTree then
        good (Good tr.Value)
      else
        good (Bad (collapse tr.ErrorTree))
#endif

  let inline junpack (ok : 'T -> JTransform<'U>) (bad : JErrorItem [] -> JTransform<'U>) (t : JTransform<'T>) =
    jtrans <| fun j p ->
      let tr = invoke t j p
      if isGood tr.ErrorTree then
        let tok = ok tr.Value
        invoke tok j p
      else
        let tbad = bad (collapse tr.ErrorTree)
        invoke tbad j p

  // Failures

  let inline jfailure v msg : JTransform<'T> =
    jtrans <| fun j p ->
      result v (msg |> JError.Message |> leaf p)

  let inline jfailuref v fmt = kprintf (jfailure v) fmt

  let inline jwarning v msg : JTransform<'T> =
    jtrans <| fun j p ->
      result v (msg |> JError.Message |> leaf p |> supp)

  let inline jwarningf v fmt = kprintf (jwarning v) fmt

  // Misc

  let inline jwithContext name (t : JTransform<'T>) : JTransform<'T> =
    jtrans <| fun j p ->
      let np = (JContextElement.Named name)::p
      invoke t j np

  let inline jdebug name (t : JTransform<'T>) : JTransform<'T> =
    jtrans <| fun j p ->
      // TODO: Print shallow json data
      printfn "BEFORE  %s: %A" name p
      let tr = invoke t j p
      if isGood tr.ErrorTree then
        printfn "SUCCESS %s: %A(%A)" name tr.Value tr.ErrorTree
      else
        printfn "FAILURE %s: %A(%A)" name tr.Value tr.ErrorTree
      tr

  let inline jrun (t : JTransform<'T>) (root : Json) : 'T*JErrorItem [] =
    let tr = invoke t root []
    tr.Value, collapse tr.ErrorTree

  // Extractors

  let jisNull : JTransform<bool> =
    jtrans <| fun j p ->
      good <|
        match j with
        | JsonNull      -> true
        | JsonBoolean _
        | JsonNumber  _
        | JsonString  _
        | JsonArray   _
        | JsonObject  _ -> false

  let jbool : JTransform<bool> =
    jtrans <| fun j p ->
      match j with
      | JsonBoolean v -> v |> good
      | JsonNull
      | JsonNumber  _
      | JsonString  _
      | JsonArray   _
      | JsonObject  _ -> result false (JError.NotABool |> leaf p)

  let jfloat : JTransform<float> =
    jtrans <| fun j p ->
      match j with
      | JsonNumber  v -> v |> good
      | JsonNull
      | JsonBoolean _
      | JsonString  _
      | JsonArray   _
      | JsonObject  _ -> result 0. (JError.NotAFloat |> leaf p)

  let jstring : JTransform<string> =
    jtrans <| fun j p ->
      match j with
      | JsonString  v -> v |> good
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _
      | JsonArray   _
      | JsonObject  _ -> result "" (JError.NotAString |> leaf p)

  let jvalue : JTransform<Json> =
    jtrans <| fun j p ->
      good j

  let jasBool : JTransform<bool> =
    jtrans <| fun j p ->
      good <|
        match j with
        | JsonNull      -> false
        | JsonBoolean v -> v
        | JsonNumber  v -> v <> 0.
        | JsonString  v -> v.Length > 0
        | JsonArray   _
        | JsonObject  _ -> false

  let jasFloat : JTransform<float> =
    jtrans <| fun j p ->
      match j with
      | JsonNull      -> 0. |> good
      | JsonBoolean v -> (if v then 1. else 0.) |> good
      | JsonNumber  v -> v |> good
      | JsonString  v ->
        let b, f = Double.TryParse (v, NumberStyles.Float, CultureInfo.InvariantCulture)
        if b then f |> good else result 0. (JError.CanNotConvertToFloat |> leaf p)
      | JsonArray   _
      | JsonObject  _ -> result 0. (JError.CanNotConvertToFloat |> leaf p)

  let jasString : JTransform<string> =
    jtrans <| fun j p ->
      match j with
      | JsonNull      -> "" |> good
      | JsonBoolean v -> (if v then "true" else "false") |> good
      | JsonNumber  v -> v.ToString CultureInfo.InvariantCulture |> good
      | JsonString  v -> v |> good
      | JsonArray   _
      | JsonObject  _ -> result "" (JError.CanNotConvertToString |> leaf p)

  // Queries

  let inline jindex idx v (t : JTransform<'T>) : JTransform<'T> =
    let inline jindex idx dv t p m (ms : _ []) =
      if idx >= 0 && idx < ms.Length then
        let v = m ms.[idx]
        let ip = (JContextElement.Index idx)::p
        invoke t v ip
      else
        result dv (idx |> JError.IndexOutOfRange |> leaf p)
    jtrans <| fun j p ->
      match j with
      | Json.JsonObject ms ->
        jindex idx v t p snd ms
      | Json.JsonArray vs ->
        jindex idx v t p id vs
      | _ ->
        result v (JError.NotAnArrayOrObject |> leaf p)

  let inline jindexz idx t =
    jindex idx (zero ()) t

  let inline jmany (t : JTransform<'T>) : JTransform<'T []> =
    jtrans <| fun j p ->
      match j with
      | Json.JsonObject ms ->
        let r = ResizeArray ms.Length
        let et = Loops.jmany t p r ms snd empty 0
        result (r.ToArray ()) et
      | Json.JsonArray vs ->
        let r = ResizeArray vs.Length
        let et = Loops.jmany t p r vs id empty 0
        result (r.ToArray ()) et
      | _ ->
        result [||] (JError.NotAnArrayOrObject |> leaf p)

  let inline jmember name v (t : JTransform<'T>) : JTransform<'T> =
    jtrans <| fun j p ->
      match j with
      | Json.JsonObject ms ->
        Loops.jmember name v t p ms 0
      | _ ->
        result v (JError.NotAnObject |> leaf p)

  let inline jmemberz name t =
    jmember name (zero ()) t

  type JBuilder () =
    member inline j.Bind        (t, uf) = jbind t uf
    member inline j.Combine     (t, u)  = jkeepRight t u
    member inline j.Return      v       = jreturn v
    member inline j.ReturnFrom  t       = t : JTransform<'T>
    member inline j.Zero        ()      = jreturn (zero ())

  module Infixes =
    let inline (>>=)  t uf  = jbind      t uf
    let inline (>=>) tf uf  = jkleisli  tf uf
    let inline (<*>) tf u   = japply    tf u
    let inline (|>>)  t m   = jmap       m t
    let inline (<|>)  l r   = jorElse    l r
    let inline (.>>.) l r   = jpair      l r
    let inline (.>>)  l r   = jkeepLeft  l r
    let inline (>>.)  l r   = jkeepRight l r

let jtransform = JTransform.JBuilder ()
