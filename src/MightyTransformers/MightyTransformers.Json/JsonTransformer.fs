module MightyTransformers.Json.JsonTransformer

// TODO: Internalize Loops and Details

open System
open System.Globalization
open System.Text

open MiniJson.JsonModule
open MiniJson.DynamicJsonModule

open MightyTransformers.Common

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
  | Failure               of string
  | IndexOutOfRange       of int
  | MemberNotFound        of string
  | NotAnArrayOrObject
  | NotABool
  | NotAFloat
  | NotAString
  | NotAnObject
  | Warning               of string

[<RequireQualifiedAccess>]
type JErrorTree =
  | Empty
  | Leaf  of JContext*JError
  | Fork  of JErrorTree*JErrorTree

type JResult<'T>(v : 'T, et : JErrorTree) =
  struct
    member x.Value      = v
    member x.ErrorTree  = et
  end

type JTransform<'T> = Json -> JContext -> JResult<'T>

module Details =

  module Loops =
    let rec pathToString (sb : StringBuilder) p =
      match p with
      | []    -> sb.Append "root" |> ignore
      | h::t  ->
        pathToString sb t
        match h with
        | JContextElement.Member n  -> sb.Append (sprintf ".%s"   n)  |> ignore
        | JContextElement.Index  i  -> sb.Append (sprintf ".[%d]" i)  |> ignore
        | JContextElement.Named  n  -> sb.Append (sprintf "(%s)"  n)  |> ignore

  let defaultSize     = 16
  let inline adapt f  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f

  let inline zero ()  = LanguagePrimitives.GenericZero<_>

  let inline leaf p e = JErrorTree.Leaf (p, e)

  let inline join l r =
    match l, r with
    | JErrorTree.Empty  , _                 -> r
    | _                 , JErrorTree.Empty  -> l
    | _                 , _                 -> JErrorTree.Fork (l, r)

  let inline invoke (t : OptimizedClosures.FSharpFunc<Json,JContext,JResult<_>>) e p = t.Invoke (e, p)

  let pathToString p =
    let sb = System.Text.StringBuilder defaultSize
    Loops.pathToString sb p
    sb.ToString ()

  module Loops2 =
    let rec collapse (result : ResizeArray<_>) et =
      match et with
      | JErrorTree.Empty         -> ()
      | JErrorTree.Leaf  (p, e)  -> result.Add (pathToString p, e)
      | JErrorTree.Fork  (l, r)  -> collapse result l; collapse result r

  let collapse (et : JErrorTree) =
    let result = ResizeArray defaultSize
    Loops2.collapse result et
    result.ToArray ()

  let inline result  v et = JResult (v, et)

  let inline good    v    = result v JErrorTree.Empty

module JTransform =
  open FSharp.Core.Printf
  open MightyTransformers.Common
  open Details

  // Monad

  let inline jreturn v : JTransform<'T> =
    fun j p ->
      good v

  let inline jbind (t : JTransform<'T>) (uf : 'T -> JTransform<'U>) : JTransform<'U> =
    let t = adapt t
    fun j p ->
      let tr  = invoke t j p
      let u   = uf tr.Value
      let u   = adapt u
      let ur  = invoke u j p
      result ur.Value (join tr.ErrorTree ur.ErrorTree)

  // Kleisli

  let inline jarr f = fun v -> jreturn (f v)

  let inline jkleisli tf uf =
    fun v -> jbind (tf v) uf

  // Applicative

  let inline jpure v = jreturn v

  let inline japply (t : JTransform<'U -> 'V>) (u : JTransform<'U>) : JTransform<'V> =
    let t = adapt t
    let u = adapt u
    fun j p ->
      let tr  = invoke t j p
      let ur  = invoke u j p
      result (tr.Value ur.Value) (join tr.ErrorTree ur.ErrorTree)

  // Functor

  let inline jmap m (t : JTransform<'T>) : JTransform<'U> =
    let t = adapt t
    fun j p ->
      let tr = invoke t j p
      result (m tr.Value) tr.ErrorTree

  // Combinators

  let inline jorElse (l : JTransform<'T>) (r : JTransform<'T>) : JTransform<'T> =
    let l = adapt l
    let r = adapt r
    fun j p ->
      let lr = invoke l j p
      match lr.ErrorTree with
      | JErrorTree.Empty  -> lr
      | _                 ->
        let rr = invoke r j p
        match rr.ErrorTree with
        | JErrorTree.Empty -> rr
        | _     ->
          result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline jkeepLeft (l : JTransform<'T>) (r : JTransform<_>) : JTransform<'T> =
    let l = adapt l
    let r = adapt r
    fun j p ->
      let lr = invoke l j p
      let rr = invoke r j p
      result lr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline jkeepRight (l : JTransform<_>) (r : JTransform<'T>) : JTransform<'T> =
    let l = adapt l
    let r = adapt r
    fun j p ->
      let lr = invoke l j p
      let rr = invoke r j p
      result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline jpair (l : JTransform<'T>) (r : JTransform<'U>) : JTransform<'T*'U> =
    let l = adapt l
    let r = adapt r
    fun j p ->
      let lr = invoke l j p
      let rr = invoke r j p
      result (lr.Value, rr.Value) (join lr.ErrorTree rr.ErrorTree)

  let inline jtoOption (t : JTransform<'T>) : JTransform<'T option> =
    let t = adapt t
    fun j p ->
      let tr = invoke t j p
      match tr.ErrorTree with
      | JErrorTree.Empty  -> good (Some tr.Value)
      | _                 -> good None

  let inline jtoResult (t : JTransform<'T>) : JTransform<Result<'T, (string*JError) []>> =
    let t = adapt t
    fun j p ->
      let tr = invoke t j p
      match tr.ErrorTree with
      | JErrorTree.Empty  -> good (Good tr.Value)
      | _                 -> good (Bad (collapse tr.ErrorTree))

  // Failures

  let inline jfailure v msg : JTransform<'T> =
    fun j p ->
      result v (msg |> JError.Failure |> leaf p)

  let inline jfailuref v fmt = kprintf (jfailure v) fmt

  let inline jwarning v msg : JTransform<'T> =
    fun j p ->
      result v (msg |> JError.Warning |> leaf p)

  let inline jwarningf v fmt = kprintf (jwarning v) fmt

  // Misc

  let inline jwithContext name (t : JTransform<'T>) : JTransform<'T> =
    let t = adapt t
    fun j p ->
      let np = (JContextElement.Named name)::p
      invoke t j np

  let inline jdebug name (t : JTransform<'T>) : JTransform<'T> =
    let t = adapt t
    fun j p ->
      // TODO: Print shallow json data
      printfn "BEFORE  %s: %A" name p
      let tr = invoke t j p
      match tr.ErrorTree with
      | JErrorTree.Empty  -> printfn "SUCCESS %s: %A" name tr.Value
      | _                 -> printfn "FAILURE %s: %A(%A)" name tr.Value tr.ErrorTree
      tr

  let inline jrun (t : JTransform<'T>) (root : Json) =
    let t = adapt t
    let tr = invoke t root []
    tr.Value, collapse tr.ErrorTree

  // Extractors

  let jisNull : JTransform<bool> =
    fun j p ->
      good <|
        match j with
        | JsonNull      -> true
        | JsonBoolean _
        | JsonNumber  _
        | JsonString  _
        | JsonArray   _
        | JsonObject  _ -> false

  let jbool : JTransform<bool> =
    fun j p ->
      match j with
      | JsonBoolean v -> v |> good
      | JsonNull
      | JsonNumber  _
      | JsonString  _
      | JsonArray   _
      | JsonObject  _ -> result false (JError.NotABool |> leaf p)

  let jfloat : JTransform<float> =
    fun j p ->
      match j with
      | JsonNumber  v -> v |> good
      | JsonNull
      | JsonBoolean _
      | JsonString  _
      | JsonArray   _
      | JsonObject  _ -> result 0. (JError.NotAFloat |> leaf p)

  let jstring : JTransform<string> =
    fun j p ->
      match j with
      | JsonString  v -> v |> good
      | JsonNull
      | JsonBoolean _
      | JsonNumber  _
      | JsonString  _
      | JsonArray   _
      | JsonObject  _ -> result "" (JError.NotAString |> leaf p)

  let jvalue : JTransform<Json> =
    fun j p ->
      good j

  let jasBool : JTransform<bool> =
    fun j p ->
      good <|
        match j with
        | JsonNull      -> false
        | JsonBoolean v -> v
        | JsonNumber  v -> v <> 0.
        | JsonString  v -> v.Length > 0
        | JsonArray   _
        | JsonObject  _ -> false

  let jasFloat : JTransform<float> =
    fun j p ->
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
    fun j p ->
      match j with
      | JsonNull      -> "" |> good
      | JsonBoolean v -> (if v then "true" else "false") |> good
      | JsonNumber  v -> v.ToString CultureInfo.InvariantCulture |> good
      | JsonString  v -> v |> good
      | JsonArray   _
      | JsonObject  _ -> result "" (JError.CanNotConvertToString |> leaf p)

  // Queries

  module Loops =
    let rec jmany t p (r : ResizeArray<_>) (ms : _ []) m et i =
      if i < ms.Length then
        let v = m ms.[i]
        let ip = (JContextElement.Index i)::p
        let tr = invoke t v ip
        match tr.ErrorTree with
        | JErrorTree.Empty -> r.Add tr.Value
        | _     -> ()
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

  let inline jindex idx v (t : JTransform<'T>) : JTransform<'T> =
    let t = adapt t
    // TODO: Check IL to ensure no unnecessary objects created
    let inline jindex idx dv t p m (ms : _ []) =
      if idx >= 0 && idx < ms.Length then
        let v = m ms.[idx]
        let ip = (JContextElement.Index idx)::p
        invoke t v ip
      else
        result dv (idx |> JError.IndexOutOfRange |> leaf p)
    fun j p ->
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
    let t = adapt t
    fun j p ->
      match j with
      | Json.JsonObject ms ->
        let r = ResizeArray ms.Length
        let et = Loops.jmany t p r ms snd JErrorTree.Empty 0
        result (r.ToArray ()) et
      | Json.JsonArray vs ->
        let r = ResizeArray vs.Length
        let et = Loops.jmany t p r vs id JErrorTree.Empty 0
        result (r.ToArray ()) et
      | _ ->
        result [||] (JError.NotAnArrayOrObject |> leaf p)

  let inline jmember name v (t : JTransform<'T>) : JTransform<'T> =
    let t = adapt t
    fun j p ->
      match j with
      | Json.JsonObject ms ->
        Loops.jmember name v t p ms 0
      | _ ->
        result v (JError.NotAnObject |> leaf p)

  let inline jmemberz name t =
    jmember name (zero ()) t

  type JBuilder() =
    member inline j.Bind        (t, uf) = jbind t uf
    member inline j.Return      v       = jreturn v
    member inline j.ReturnFrom  t       = t : JTransform<'T>

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
