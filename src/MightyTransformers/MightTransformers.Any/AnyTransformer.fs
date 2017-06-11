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

module MightyTransformers.Any.AnyTransformer

[<Struct>]
type Maybe<'T> =
  | Just    of 'T
  | Nothing

[<NoEquality>]
[<NoComparison>]
type MaybeKleisli<'T, 'U> = MaybeKleisli of ('T -> Maybe<'U>)

module Maybe =

  // Monad

  let inline mreturn v : Maybe<_> = Just v
  [<GeneralizableValue>]
  let mempty<'T> = Nothing
  let inline mbind t uf : Maybe<_> =
    match t with
    | Just tv -> uf tv
    | Nothing -> Nothing

  // Kleisli

  let inline marr f         : MaybeKleisli<_, _>  = MaybeKleisli <| fun v -> mreturn (f v)
  let inline mkleisli tf uf : MaybeKleisli<_, _>  = MaybeKleisli <| fun v -> mbind (tf v) uf

  // Applicative

  let inline mpure v = mreturn v
  let inline mapply f t : Maybe<_> =
    match f, t with
    | Just fv, Just tv  -> fv tv |> Just
    | _      , _        -> Nothing

  // Functor

  let inline mmap m t : Maybe<_> =
    match t with
    | Just tv -> m tv |> Just
    | Nothing -> Nothing

  // Combinators

  let inline mand t u : Maybe<_*_> =
    match t, u with
    | Just tv, Just uv  -> (tv, uv) |> Just
    | _      , _        -> Nothing
  let inline mor t u : Maybe<_> =
    match t with
    | Just _  -> t
    | Nothing -> u

  [<NoEquality>]
  [<NoComparison>]
  type MaybeBuilder () =
    member inline x.Bind        (t, uf) = mbind   t uf
    member inline x.Return      v       = mreturn v
    member inline x.ReturnFrom  t       = t             : Maybe<_>
    member inline x.Zero        ()      = mempty

type Maybe<'T> with
  static member inline (>>=) (t , uf) = Maybe.mbind   t uf
  static member inline (<*>) (tf, u)  = Maybe.mapply tf u
  static member inline (|>>) (t , m)  = Maybe.mmap    m t
  static member inline (<&>) (l , r)  = Maybe.mand    l r
  static member inline (<|>) (l , r)  = Maybe.mor     l r

type MaybeKleisli<'T, 'U> with
  static member inline (>=>) (tf, uf) = Maybe.mkleisli tf uf

let maybe = Maybe.MaybeBuilder ()

[<Struct>]
type LazyList<'T> =
  | LazyEmpty
  | LazyCons of 'T*(unit -> LazyList<'T>)

module LazyList =
  let lempty = LazyEmpty
  let inline lcons v ll = LazyCons (v, ll)

  let rec lfromArrayi i (s : 'T []) : LazyList<'T> =
    if s.Length > i && i >= 0 then
      LazyCons (s.[i], fun () -> lfromArrayi (i + 1) s)
    else
      LazyEmpty

  let lfromArray s = lfromArrayi 0 s

open System
open System.Collections.Generic
open System.Globalization

type IAnyTree =
  interface
    abstract CanIndex   : bool
    abstract CanLookup  : bool
    abstract CanIterate : bool
    abstract Value      : Maybe<obj>
    abstract Type       : Type
    abstract Index      : int     -> Maybe<IAnyTree>
    abstract Lookup     : string  -> Maybe<IAnyTree>
    abstract Iterator   : unit    -> LazyList<IAnyTree>
  end

module AnyTree =
  module Adapter =
    let rec adaptObj (o : obj): IAnyTree =
      match o with
      | null                      ->
        { new IAnyTree with
          member x.CanIndex       = false
          member x.CanLookup      = false
          member x.CanIterate     = false
          member x.Value          = Nothing
          member x.Type           = typeof<Void>
          member x.Index    idx   = Nothing
          member x.Lookup   name  = Nothing
          member x.Iterator ()    = LazyList.lempty
        }
      | :? Map<string, obj> as v  -> adaptMap v
      | :? (obj []) as v          -> adaptArray v
      | _                         ->
        { new IAnyTree with
          member x.CanIndex       = false
          member x.CanLookup      = false
          member x.CanIterate     = false
          member x.Value          = Just o
          member x.Type           = o.GetType ()
          member x.Index    idx   = Nothing
          member x.Lookup   name  = Nothing
          member x.Iterator ()    = LazyList.lempty
        }
    and internal adaptMap (m : Map<string, obj>) : IAnyTree =
      { new IAnyTree with
        member x.CanIndex       = false
        member x.CanLookup      = true
        member x.CanIterate     = true
        member x.Value          = Nothing
        member x.Type           = typeof<Map<string, obj>>
        member x.Index    idx   = Nothing
        member x.Lookup   name  =
          match m.TryFind name with
          | Some v  -> Just (adaptObj v)
          | None    -> Nothing
        member x.Iterator ()    = m |> Seq.map (fun kv -> adaptObj kv.Value) |> Seq.toArray |> LazyList.lfromArray
      }
    and internal adaptArray (a : obj []) : IAnyTree =
      { new IAnyTree with
        member x.CanIndex       = true
        member x.CanLookup      = false
        member x.CanIterate     = true
        member x.Value          = Nothing
        member x.Type           = typeof<obj []>
        member x.Index    idx   =
          if idx < a.Length && idx >= 0 then
            Just (adaptObj a.[idx])
          else
            Nothing
        member x.Lookup   name  = Nothing
        member x.Iterator ()    = a |> Array.map adaptObj |> LazyList.lfromArray
      }

[<RequireQualifiedAccess>]
type AnyContextElement =
  | Member  of  string
  | Index   of  int
  | Named   of  string
[<Struct>]
type AnyContext = AnyContext of AnyContextElement list

[<RequireQualifiedAccess>]
type AnyError =
  | CanNotConvertTo of obj*Type*Type
  | IndexOutOfRange of int
  | MemberNotFound  of string
  | NoValue         of Type
  | NotIndexable    of Type
  | NotLookupable   of Type
  | NotIterable     of Type
  | Message         of string

[<RequireQualifiedAccess>]
type AnyErrorTree =
  | Empty
  | Leaf      of AnyContext*AnyError
  | Suppress  of AnyErrorTree
  | Fork      of AnyErrorTree*AnyErrorTree

[<Struct>]
type AnyResult<'T> =
  | AnyResult of 'T*AnyErrorTree

  member x.Value =
    let (AnyResult (v, _)) = x
    v

  member x.ErrorTree =
    let (AnyResult (_, et)) = x
    et

[<Struct>]
type AnyErrorItem =
  {
    IsSuppressed  : bool
    Path          : string
    Error         : AnyError
  }
  static member New i p e : AnyErrorItem = { IsSuppressed = i; Path = p; Error = e }

//

[<NoEquality>]
[<NoComparison>]
[<Struct>]
type AnyTransform<'T> = AnyTransform of OptimizedClosures.FSharpFunc<IAnyTree, AnyContext, AnyResult<'T>>

[<NoEquality>]
[<NoComparison>]
type AnyTransformKleisli<'T, 'U> = AnyTransformKleisli of ('T -> AnyTransform<'U>)

module AnyTransform =
  open FSharp.Core.Printf

  module Details =
    open System.Text

    let defaultSize     = 16

    let defaultCulture  = CultureInfo.InvariantCulture

    module TypeMapper =
      type Mapper<'From, 'To> = ('From -> Maybe<'To>)

      type TypeMapper<'T> =
        {
          Zero        : 'T
          AsString    : 'T -> string
          FromString  : Mapper<string   , 'T>
          FromByte    : Mapper<byte     , 'T>
          FromChar    : Mapper<char     , 'T>
          FromDecimal : Mapper<decimal  , 'T>
          FromFloat   : Mapper<float    , 'T>
          FromFloat32 : Mapper<float32  , 'T>
          FromInt16   : Mapper<int16    , 'T>
          FromInt32   : Mapper<int32    , 'T>
          FromInt64   : Mapper<int64    , 'T>
          FromSByte   : Mapper<sbyte    , 'T>
          FromUInt16  : Mapper<uint16   , 'T>
          FromUInt32  : Mapper<uint32   , 'T>
          FromUInt64  : Mapper<uint64   , 'T>
        }

        static member New zero asString fromString fromByte fromChar fromDecimal fromFloat fromFloat32 fromInt16 fromInt32 fromInt64 fromSByte fromUInt16 fromUInt32 fromUInt64 =
          {
            Zero        = zero
            AsString    = asString
            FromString  = fromString
            FromByte    = fromByte
            FromChar    = fromChar
            FromDecimal = fromDecimal
            FromFloat   = fromFloat
            FromFloat32 = fromFloat32
            FromInt16   = fromInt16
            FromInt32   = fromInt32
            FromInt64   = fromInt64
            FromSByte   = fromSByte
            FromUInt16  = fromUInt16
            FromUInt32  = fromUInt32
            FromUInt64  = fromUInt64
          } : TypeMapper<_>

      let parseDecimal s                =
        let ns = NumberStyles.Float
        match Decimal.TryParse(s, ns, defaultCulture) with
        | true, v -> v |> Just
        | _   , _ -> Nothing

      let parseFloat m s                =
        let ns = NumberStyles.Float
        match Double.TryParse(s, ns, defaultCulture) with
        | true, v -> v |> m |> Just
        | _   , _ -> Nothing

      let parseInt m s                  =
        let ns = NumberStyles.Integer
        match Int64.TryParse(s, ns, defaultCulture) with
        | true, v -> v |> m |> Just
        | _   , _ -> Nothing

      let parseUInt m s                  =
        let ns = NumberStyles.Integer
        match UInt64.TryParse(s, ns, defaultCulture) with
        | true, v -> v |> m |> Just
        | _   , _ -> Nothing

      let asString (v : #IFormattable)  = v.ToString("", defaultCulture)
      let fromString v                  = Nothing

      let doMap t f v =
        let tv  = t v
        let ftv = f tv
        if ftv = v then Just tv else Nothing

      let mdtoc, mctod =
        let dtoc d = d |> int |> char
        let ctod c = c |> int |> decimal
        doMap dtoc ctod, doMap ctod dtoc

      let ctos (c : char) = c.ToString ()

      let tm_byte     = let inline m m = doMap byte    m in TypeMapper<_>.New (byte    0) asString (parseUInt byte    ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_char     = let inline m m = doMap char    m in TypeMapper<_>.New (char    0) ctos     (parseInt char     ) (m byte) (m char) (mdtoc    ) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_decimal  = let inline m m = doMap decimal m in TypeMapper<_>.New (decimal 0) asString (parseDecimal      ) (m byte) (mctod ) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_float    = let inline m m = doMap float   m in TypeMapper<_>.New (float   0) asString (parseFloat float  ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_float32  = let inline m m = doMap float32 m in TypeMapper<_>.New (float32 0) asString (parseFloat float32) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_int16    = let inline m m = doMap int16   m in TypeMapper<_>.New (int16   0) asString (parseInt int16    ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_int32    = let inline m m = doMap int32   m in TypeMapper<_>.New (int32   0) asString (parseInt int32    ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_int64    = let inline m m = doMap int64   m in TypeMapper<_>.New (int64   0) asString (parseInt int64    ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_sbyte    = let inline m m = doMap sbyte   m in TypeMapper<_>.New (sbyte   0) asString (parseInt sbyte    ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_uint16   = let inline m m = doMap uint16  m in TypeMapper<_>.New (uint16  0) asString (parseUInt uint16  ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_uint32   = let inline m m = doMap uint32  m in TypeMapper<_>.New (uint32  0) asString (parseUInt uint32  ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)
      let tm_uint64   = let inline m m = doMap uint64  m in TypeMapper<_>.New (uint64  0) asString (parseUInt uint64  ) (m byte) (m char) (m decimal) (m float) (m float32) (m int16) (m int32) (m int64) (m sbyte) (m uint16) (m uint32) (m uint64)

    let inline ttrans f = AnyTransform(OptimizedClosures.FSharpFunc<_, _, _>.Adapt f)

    let inline zero ()  = LanguagePrimitives.GenericZero<_>

    let inline leaf c e = AnyErrorTree.Leaf (c, e)

    let empty           = AnyErrorTree.Empty

    let inline isGood e=
      match e with
      | AnyErrorTree.Empty
      | AnyErrorTree.Suppress _ -> true
      | _                       -> false

    let inline supp e   =
      match e with
      | AnyErrorTree.Empty
      | AnyErrorTree.Suppress _ -> e
      | _                       -> AnyErrorTree.Suppress e

    let inline join l r =
      match l, r with
      | AnyErrorTree.Empty      , _                       -> r
      | _                       , AnyErrorTree.Empty      -> l
      | AnyErrorTree.Suppress l , AnyErrorTree.Suppress r -> AnyErrorTree.Fork (l, r) |> AnyErrorTree.Suppress
      | _                       , _                       -> AnyErrorTree.Fork (l, r)

    let inline invoke (AnyTransform f) t c = f.Invoke (t, c)

    let pathToString (AnyContext p) =
      // Verified that call to private pathToString don't do "funny" stuff
      let rec pathToString (sb : StringBuilder) p =
        match p with
        | []    -> sb.Append "json" |> ignore
        | h::t  ->
          pathToString sb t
          match h with
          | AnyContextElement.Member n  -> sb.Append (sprintf ".%s"   n)  |> ignore
          | AnyContextElement.Index  i  -> sb.Append (sprintf ".[%d]" i)  |> ignore
          | AnyContextElement.Named  n  -> sb.Append (sprintf "(%s)"  n)  |> ignore
      let sb = System.Text.StringBuilder defaultSize
      pathToString sb p
      sb.ToString ()

    let collapse (et : AnyErrorTree) =
      // Verified that call to private collapse don't do "funny" stuff
      let rec collapse suppress (result : ResizeArray<_>) et =
        match et with
        | AnyErrorTree.Empty            -> ()
        | AnyErrorTree.Leaf     (c, e)  -> AnyErrorItem.New suppress (pathToString c) e |> result.Add
        | AnyErrorTree.Suppress e       -> collapse true result e
        | AnyErrorTree.Fork     (l, r)  -> collapse suppress result l; collapse suppress result r

      let result = ResizeArray defaultSize
      collapse false result et
      result.ToArray ()

    let inline result  v et = AnyResult (v, et)

    let inline good    v    = result v empty

    module Loops =
      let rec tmany t c (ra : ResizeArray<_>) lazyList et i =
        let (AnyContext p)        = c
        match lazyList with
        | LazyCons (v, ll) ->
          let ic = (AnyContextElement.Index i)::p |> AnyContext
          let tr = invoke t v ic
          if isGood tr.ErrorTree then
            ra.Add tr.Value
          tmany t c ra (ll ()) (join et tr.ErrorTree) (i + 1)
        | LazyEmpty ->
          et

  open Details

  // Monad

  let inline treturn v : AnyTransform<'T> =
    ttrans <| fun at c ->
      good v
  let inline tbind (t : AnyTransform<'T>) (uf : 'T -> AnyTransform<'U>) : AnyTransform<'U> =
    ttrans <| fun at c ->
      let tr  = invoke t at c
      let u   = uf tr.Value
      let ur  = invoke u at c
      result ur.Value (join tr.ErrorTree ur.ErrorTree)

  // Kleisli

  let inline tarr f = AnyTransformKleisli <| fun v -> treturn (f v)
  let inline tkleisli tf uf = AnyTransformKleisli <| fun v -> tbind (tf v) uf

  // Applicative

  let inline tpure v = treturn v
  let inline tapply (t : AnyTransform<'U -> 'V>) (u : AnyTransform<'U>) : AnyTransform<'V> =
    ttrans <| fun at c ->
      let tr  = invoke t at c
      let ur  = invoke u at c
      result (tr.Value ur.Value) (join tr.ErrorTree ur.ErrorTree)

  // Functor

  let inline tmap m (t : AnyTransform<'T>) : AnyTransform<'U> =
    ttrans <| fun at c ->
      let tr = invoke t at c
      result (m tr.Value) tr.ErrorTree

  // Combinators

  let inline tand (lt : AnyTransform<'T>) (rt : AnyTransform<'U>) : AnyTransform<'T*'U> =
    ttrans <| fun at c ->
      let lr = invoke lt at c
      let rr = invoke rt at c
      result (lr.Value, rr.Value) (join lr.ErrorTree rr.ErrorTree)
  let inline tor (lt : AnyTransform<'T>) (rt : AnyTransform<'T>) : AnyTransform<'T> =
    ttrans <| fun at c ->
      let lr = invoke lt at c
      if isGood lr.ErrorTree then
        lr
      else
        let rr = invoke rt at c
        if isGood rr.ErrorTree then
          rr
        else
          result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline tkeepLeft (lt : AnyTransform<'T>) (rt : AnyTransform<_>) : AnyTransform<'T> =
    ttrans <| fun at c ->
      let lr = invoke lt at c
      let rr = invoke rt at c
      result lr.Value (join lr.ErrorTree rr.ErrorTree)
  let inline tkeepRight (lt : AnyTransform<_>) (rt : AnyTransform<'T>) : AnyTransform<'T> =
    ttrans <| fun at c ->
      let lr = invoke lt at c
      let rr = invoke rt at c
      result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline tsuppress (t : AnyTransform<'T>) : AnyTransform<'T> =
    ttrans <| fun at c ->
      let tr = invoke t at c
      let e  =
        if isGood tr.ErrorTree then
          tr.ErrorTree
        else
          tr.ErrorTree |> supp
      result tr.Value e

  let inline ttoMaybe (t : AnyTransform<'T>) : AnyTransform<Maybe<'T>> =
    ttrans <| fun at c ->
      let tr = invoke t at c
      if isGood tr.ErrorTree then
        good (Just tr.Value)
      else
        good Nothing

  let inline ttoOption (t : AnyTransform<'T>) : AnyTransform<'T option> =
    ttrans <| fun at c ->
      let tr = invoke t at c
      if isGood tr.ErrorTree then
        good (Some tr.Value)
      else
        good None

#if !FSHARP_41
  let inline ttoResult (t : AnyTransform<'T>) : AnyTransform<Result<'T, AnyErrorItem []>> =
    ttrans <| fun at c ->
      let tr = invoke t at c
      if isGood tr.ErrorTree then
        good (Ok tr.Value)
      else
        good (Error (collapse tr.ErrorTree))
#endif

  let inline tunpack (ok : 'T -> AnyTransform<'U>) (bad : AnyErrorItem [] -> AnyTransform<'U>) (t : AnyTransform<'T>) =
    ttrans <| fun at c ->
      let tr = invoke t at c
      if isGood tr.ErrorTree then
        let tok = ok tr.Value
        invoke tok at c
      else
        let tbad = bad (collapse tr.ErrorTree)
        invoke tbad at c

  // Failures

  let inline tfailure v msg : AnyTransform<'T> =
    ttrans <| fun at c ->
      result v (msg |> AnyError.Message |> leaf c)

  let inline tfailuref v fmt = kprintf (tfailure v) fmt

  let inline twarning v msg : AnyTransform<'T> =
    ttrans <| fun at c ->
      result v (msg |> AnyError.Message |> leaf c |> supp)

  let inline twarningf v fmt = kprintf (twarning v) fmt

  // Misc

  let inline twithContext name (t : AnyTransform<'T>) : AnyTransform<'T> =
    ttrans <| fun at (AnyContext p) ->
      let nc = (AnyContextElement.Named name)::p |> AnyContext
      invoke t at nc

  let inline tdebug name (t : AnyTransform<'T>) : AnyTransform<'T> =
    ttrans <| fun at c ->
      printfn "BEFORE  %s: %A(%A)" name at c
      let tr = invoke t at c
      if isGood tr.ErrorTree then
        printfn "SUCCESS %s: %A(%A)" name tr.Value tr.ErrorTree
      else
        printfn "FAILURE %s: %A(%A)" name tr.Value tr.ErrorTree
      tr

  let trun (t : AnyTransform<'T>) (root : IAnyTree) : 'T*AnyErrorItem [] =
    let tr = invoke t root ([] |> AnyContext)
    tr.Value, collapse tr.ErrorTree

  // Extractors

  let inline textract (tm : TypeMapper.TypeMapper<'T>) : AnyTransform<'T> =
    ttrans <| fun at c ->
      match at.Value with
      | Just v ->
        let mv =
          match v with
          | null                    -> Nothing
          | (:? string  as v)       -> tm.FromString  v
          | (:? byte    as v)       -> tm.FromByte    v
          | (:? char    as v)       -> tm.FromChar    v
          | (:? decimal as v)       -> tm.FromDecimal v
          | (:? float   as v)       -> tm.FromFloat   v
          | (:? float32 as v)       -> tm.FromFloat32 v
          | (:? int16   as v)       -> tm.FromInt16   v
          | (:? int32   as v)       -> tm.FromInt32   v
          | (:? int64   as v)       -> tm.FromInt64   v
          | (:? sbyte   as v)       -> tm.FromSByte   v
          | (:? uint16  as v)       -> tm.FromUInt16  v
          | (:? uint32  as v)       -> tm.FromUInt32  v
          | (:? uint64  as v)       -> tm.FromUInt64  v
          | (:? IFormattable as f)  -> tm.FromString  (f.ToString ("", defaultCulture))
          | _                       -> tm.FromString  (v.ToString ())
        match mv with
        | Just v  -> v |> good
        | Nothing -> result tm.Zero (AnyError.CanNotConvertTo (v, at.Type, typeof<'T>)|> leaf c)
      | Nothing -> result tm.Zero (AnyError.NoValue at.Type |> leaf c)

  let tasByte    = textract TypeMapper.tm_byte
  let tasChar    = textract TypeMapper.tm_char
  let tasDecimal = textract TypeMapper.tm_decimal
  let tasFloat   = textract TypeMapper.tm_float
  let tasFloat32 = textract TypeMapper.tm_float32
  let tasInt16   = textract TypeMapper.tm_int16
  let tasInt32   = textract TypeMapper.tm_int32
  let tasInt64   = textract TypeMapper.tm_int64
  let tasSByte   = textract TypeMapper.tm_sbyte
  let tasUInt16  = textract TypeMapper.tm_uint16
  let tasUInt32  = textract TypeMapper.tm_uint32
  let tasUInt64  = textract TypeMapper.tm_uint64

  let tasString : AnyTransform<string> =
    ttrans <| fun at c ->
      match at.Value with
      | Just v ->
        match v with
        | (:? string as v)        -> v                                |> good
        | (:? IFormattable as f)  -> f.ToString ("", defaultCulture)  |> good
        | _                       -> v |> string                      |> good
      | Nothing -> result "" (AnyError.NoValue at.Type |> leaf c)

  // Queries

  let inline tindex idx v (t : AnyTransform<'T>) : AnyTransform<'T> =
    ttrans <| fun at c ->
      if at.CanIndex then
        match at.Index idx with
        | Just v  ->
          let (AnyContext p) = c
          let ic = (AnyContextElement.Index idx)::p |> AnyContext
          invoke t v ic
        | Nothing     ->
          result v (at.Type |> AnyError.NotIndexable |> leaf c)
      else
        result v (idx |> AnyError.IndexOutOfRange |> leaf c)
  let inline tindexz idx t = tindex idx (zero ()) t

  let inline tmany (t : AnyTransform<'T>) : AnyTransform<'T []> =
    ttrans <| fun at c ->
      let ra = ResizeArray defaultSize
      if at.CanIterate then
        let et = Loops.tmany t c ra (at.Iterator ()) empty 0
        result (ra.ToArray ()) et
      else
        result [||] (at.Type |> AnyError.NotIterable |> leaf c)

  let inline tmember name v (t : AnyTransform<'T>) : AnyTransform<'T> =
    ttrans <| fun at c ->
      if at.CanLookup then
        match at.Lookup name with
        | Just v    ->
          let (AnyContext p) = c
          let ic = (AnyContextElement.Member name)::p |> AnyContext
          invoke t v ic
        | Nothing   ->
          result v (name |> AnyError.MemberNotFound |> leaf c)
      else
        result v (at.Type |> AnyError.NotLookupable |> leaf c)
  let inline tmemberz name t = tmember name (zero ()) t

  type AnyBuilder () =
    member inline x.Bind        (t, uf) = tbind       t uf
    member inline x.Combine     (t, u)  = tkeepRight  t u
    member inline x.Return      v       = treturn     v
    member inline x.ReturnFrom  t       = t                     : AnyTransform<'T>
    member inline x.Zero        ()      = treturn     (zero ())

type AnyTransform<'T> with
  static member inline (>>=) (t , uf) = AnyTransform.tbind      t uf
  static member inline (<*>) (tf, u)  = AnyTransform.tapply    tf u
  static member inline (|>>) (t , m)  = AnyTransform.tmap       m t
  static member inline (<&>) (l , r)  = AnyTransform.tand       l r
  static member inline (<|>) (l , r)  = AnyTransform.tor        l r
  static member inline (.>>) (l , r)  = AnyTransform.tkeepLeft  l r
  static member inline (>>.) (l , r)  = AnyTransform.tkeepRight l r

type AnyTransformKleisli<'T, 'U> with
  static member inline (>=>) (tf, uf) = AnyTransform.tkleisli tf uf

let anyTransform = AnyTransform.AnyBuilder ()
