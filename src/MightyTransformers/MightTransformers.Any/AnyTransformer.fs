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
[<NoEquality>]
[<NoComparison>]
type AnyIndexer<'T>   = AnyIndexer  of (int     -> Maybe<'T>)

[<Struct>]
[<NoEquality>]
[<NoComparison>]
type AnyLookup<'T>    = AnyLookup   of (string  -> Maybe<'T>)

[<Struct>]
[<NoEquality>]
[<NoComparison>]
type AnyIterator<'T>  = AnyIterator of (unit    -> Maybe<'T>)

open System
open System.Collections.Generic
open System.Globalization

[<Struct>]
[<NoEquality>]
[<NoComparison>]
type AnyAdapter     = AnyAdapter    of (obj -> obj)

[<Struct>]
[<NoEquality>]
[<NoComparison>]
type AnyAdapterGen  = AnyAdapterGen of (obj -> Maybe<AnyAdapter>)

[<NoEquality>]
[<NoComparison>]
type AnyAdapterRepository() =
  let indexerGens   = ResizeArray<AnyAdapterGen> 16
  let lookupGens    = ResizeArray<AnyAdapterGen> 16
  let iteratorGens  = ResizeArray<AnyAdapterGen> 16

  let indexers      = Dictionary<Type, AnyAdapter> ()
  let lookups       = Dictionary<Type, AnyAdapter> ()
  let iterators     = Dictionary<Type, AnyAdapter> ()

  let lookup (fs : Dictionary<Type, AnyAdapter>) (gs : ResizeArray<AnyAdapterGen>) (o : obj) : Maybe<'T> =
    if o <> null then
      let t = o.GetType ()
      match fs.TryGetValue t with
      | true  , (AnyAdapter f)  -> f o :?> 'T |> Just
      | false , _               ->
        let rec loop i =
          if i < gs.Count then
            let (AnyAdapterGen g) = gs.[i]
            match g o with
            | Just f ->
              fs.[t] <- f
              let (AnyAdapter f) = f
              f o :?> 'T |> Just
            | Nothing -> loop (i + 1)
          else
            Nothing
        loop 0
    else
      Nothing

  let add     (fs : Dictionary<Type, AnyAdapter>) t f = fs.[t] <- f
  let addGen  (gs : ResizeArray<AnyAdapterGen>)   g   = gs.Add g

  let adapt f = (fun (o : obj)-> f (o :?> 'T) :> obj) |> AnyAdapter

  member x.Indexer<'T>    (o : obj) : Maybe<AnyIndexer<'T>>     = lookup indexers  indexerGens  o
  member x.Lookup<'T>     (o : obj) : Maybe<AnyLookup<'T>>      = lookup lookups   lookupGens   o
  member x.Iterator<'T>   (o : obj) : Maybe<AnyIterator<'T>>    = lookup iterators iteratorGens o

  //member x.AddIndexer     (t : Type) (f : obj -> obj) : unit    = add indexers  t f
  //member x.AddLookup      (t : Type) (f : obj -> obj) : unit    = add lookups   t f
  //member x.AddIterator    (t : Type) (f : obj -> obj) : unit    = add iterators t f

  member x.AddIndexer<'T, 'U>   (f : 'T -> AnyIndexer<'U>)      = add indexers  typeof<'T> (adapt f)
  member x.AddLookup<'T, 'U>    (f : 'T -> AnyLookup<'U>)       = add lookups   typeof<'T> (adapt f)
  member x.AddIterator<'T, 'U>  (f : 'T -> AnyIterator<'U>)     = add iterators typeof<'T> (adapt f)

  member x.AddIndexerGen  (g : AnyAdapterGen) : unit = addGen indexerGens  g
  member x.AddLookupGen   (g : AnyAdapterGen) : unit = addGen lookupGens   g
  member x.AddIteratorGen (g : AnyAdapterGen) : unit = addGen iteratorGens g

module AnyAdapter =
  let arrayIterator (a : 'T []) : AnyIterator<'T> =
    let mutable i = 0
    AnyIterator <| fun () ->
      if i < a.Length then
        let v = Just a.[i]
        i <- i + 1
        v
      else
        Nothing

  let arrayIndexer (a : 'T []) : AnyIndexer<'T> =
    AnyIndexer <| fun i ->
      if i < a.Length then
        Just a.[i]
      else
        Nothing

  let mapLookup (m : Map<string, 'T>) : AnyLookup<'T> =
    AnyLookup <| fun name ->
      match m |> Map.tryFind name with
      | Some v  -> v |> Just
      | None    -> Nothing

  let mapIterator (m : Map<string, 'T>) : AnyIterator<'T> =
    let e = (m :> IEnumerable<KeyValuePair<string, 'T>>)
    let e = e.GetEnumerator ()
    AnyIterator <| fun () ->
      if e.MoveNext () then
        Just e.Current.Value
      else
        e.Dispose ()
        Nothing

(*
  let mapLookupGen : AnyAdapterGen =
    let genMap = typeof<Map<_, _>>.GetGenericTypeDefinition ()
    AnyAdapterGen <| fun o ->
      if o <> null then
        let t   = o.GetType ()
        if t.IsGenericType && t.GetGenericTypeDefinition () = genMap then
          Nothing
        else
          Nothing
      else
        Nothing
*)

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
type AnyTransform<'T> = AnyTransform of OptimizedClosures.FSharpFunc<obj, AnyContext, AnyAdapterRepository, AnyResult<'T>>

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

    let inline atrans f = AnyTransform(OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt f)

    let inline zero ()  = LanguagePrimitives.GenericZero<_>

    let inline getType o= if o <> null then o.GetType () else typeof<Void>

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

    let inline invoke (AnyTransform f) o c r = f.Invoke (o, c, r)

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
      let rec amany t c r (ra : ResizeArray<_>) iterator et i =
        let (AnyContext p)        = c
        let (AnyIterator getter)  = iterator
        match getter () with
        | Just v ->
          let ic = (AnyContextElement.Index i)::p |> AnyContext
          let tr = invoke t v ic r
          if isGood tr.ErrorTree then
            ra.Add tr.Value
          amany t c r ra iterator (join et tr.ErrorTree) (i + 1)
        | Nothing ->
          et

  open Details

  // Monad

  let inline areturn v : AnyTransform<'T> =
    atrans <| fun o c r ->
      good v
  let inline abind (t : AnyTransform<'T>) (uf : 'T -> AnyTransform<'U>) : AnyTransform<'U> =
    atrans <| fun o c r->
      let tr  = invoke t o c r
      let u   = uf tr.Value
      let ur  = invoke u o c r
      result ur.Value (join tr.ErrorTree ur.ErrorTree)

  // Kleisli

  let inline aarr f = AnyTransformKleisli <| fun v -> areturn (f v)
  let inline akleisli tf uf = AnyTransformKleisli <| fun v -> abind (tf v) uf

  // Applicative

  let inline apure v = areturn v
  let inline aapply (t : AnyTransform<'U -> 'V>) (u : AnyTransform<'U>) : AnyTransform<'V> =
    atrans <| fun o c r ->
      let tr  = invoke t o c r
      let ur  = invoke u o c r
      result (tr.Value ur.Value) (join tr.ErrorTree ur.ErrorTree)

  // Functor

  let inline amap m (t : AnyTransform<'T>) : AnyTransform<'U> =
    atrans <| fun o c r->
      let tr = invoke t o c r
      result (m tr.Value) tr.ErrorTree

  // Combinators

  let inline aand (lt : AnyTransform<'T>) (rt : AnyTransform<'U>) : AnyTransform<'T*'U> =
    atrans <| fun o c r ->
      let lr = invoke lt o c r
      let rr = invoke rt o c r
      result (lr.Value, rr.Value) (join lr.ErrorTree rr.ErrorTree)
  let inline aor (lt : AnyTransform<'T>) (rt : AnyTransform<'T>) : AnyTransform<'T> =
    atrans <| fun o c r ->
      let lr = invoke lt o c r
      if isGood lr.ErrorTree then
        lr
      else
        let rr = invoke rt o c r
        if isGood rr.ErrorTree then
          rr
        else
          result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline akeepLeft (lt : AnyTransform<'T>) (rt : AnyTransform<_>) : AnyTransform<'T> =
    atrans <| fun o c r ->
      let lr = invoke lt o c r
      let rr = invoke rt o c r
      result lr.Value (join lr.ErrorTree rr.ErrorTree)
  let inline akeepRight (lt : AnyTransform<_>) (rt : AnyTransform<'T>) : AnyTransform<'T> =
    atrans <| fun o c r ->
      let lr = invoke lt o c r
      let rr = invoke rt o c r
      result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline asuppress (t : AnyTransform<'T>) : AnyTransform<'T> =
    atrans <| fun o c r ->
      let tr = invoke t o c r
      let e  =
        if isGood tr.ErrorTree then
          tr.ErrorTree
        else
          tr.ErrorTree |> supp
      result tr.Value e

  let inline atoMaybe (t : AnyTransform<'T>) : AnyTransform<Maybe<'T>> =
    atrans <| fun o c r ->
      let tr = invoke t o c r
      if isGood tr.ErrorTree then
        good (Just tr.Value)
      else
        good Nothing

  let inline atoOption (t : AnyTransform<'T>) : AnyTransform<'T option> =
    atrans <| fun o c r ->
      let tr = invoke t o c r
      if isGood tr.ErrorTree then
        good (Some tr.Value)
      else
        good None

#if !FSHARP_41
  let inline atoResult (t : AnyTransform<'T>) : AnyTransform<Result<'T, AnyErrorItem []>> =
    atrans <| fun o c r ->
      let tr = invoke t o c r
      if isGood tr.ErrorTree then
        good (Ok tr.Value)
      else
        good (Error (collapse tr.ErrorTree))
#endif

  let inline aunpack (ok : 'T -> AnyTransform<'U>) (bad : AnyErrorItem [] -> AnyTransform<'U>) (t : AnyTransform<'T>) =
    atrans <| fun o c r ->
      let tr = invoke t o c r
      if isGood tr.ErrorTree then
        let tok = ok tr.Value
        invoke tok o c r
      else
        let tbad = bad (collapse tr.ErrorTree)
        invoke tbad o c r

  // Failures

  let inline afailure v msg : AnyTransform<'T> =
    atrans <| fun o c r ->
      result v (msg |> AnyError.Message |> leaf c)

  let inline afailuref v fmt = kprintf (afailure v) fmt

  let inline awarning v msg : AnyTransform<'T> =
    atrans <| fun o c r ->
      result v (msg |> AnyError.Message |> leaf c |> supp)

  let inline awarningf v fmt = kprintf (awarning v) fmt

  // Misc

  let inline awithContext name (t : AnyTransform<'T>) : AnyTransform<'T> =
    atrans <| fun o (AnyContext p) r ->
      let nc = (AnyContextElement.Named name)::p |> AnyContext
      invoke t o nc r

  let inline adebug name (t : AnyTransform<'T>) : AnyTransform<'T> =
    atrans <| fun o c r ->
      printfn "BEFORE  %s: %A(%A)" name o c
      let tr = invoke t o c r
      if isGood tr.ErrorTree then
        printfn "SUCCESS %s: %A(%A)" name tr.Value tr.ErrorTree
      else
        printfn "FAILURE %s: %A(%A)" name tr.Value tr.ErrorTree
      tr

  let arun (t : AnyTransform<'T>) (root : obj) (repo : AnyAdapterRepository): 'T*AnyErrorItem [] =
    let tr = invoke t root ([] |> AnyContext) repo
    tr.Value, collapse tr.ErrorTree

  // Extractors

  let aextract (tm : TypeMapper.TypeMapper<'T>) : AnyTransform<'T> =
    atrans <| fun o c r ->
      let mv =
        match o with
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
        | _                       -> tm.FromString  (o.ToString ())
      match mv with
      | Just v  -> v |> good
      | Nothing -> result tm.Zero (AnyError.CanNotConvertTo (o, getType o, typeof<'T>)|> leaf c)

  let aasByte    = aextract TypeMapper.tm_byte
  let aasChar    = aextract TypeMapper.tm_char
  let aasDecimal = aextract TypeMapper.tm_decimal
  let aasFloat   = aextract TypeMapper.tm_float
  let aasFloat32 = aextract TypeMapper.tm_float32
  let aasInt16   = aextract TypeMapper.tm_int16
  let aasInt32   = aextract TypeMapper.tm_int32
  let aasInt64   = aextract TypeMapper.tm_int64
  let aasSByte   = aextract TypeMapper.tm_sbyte
  let aasUInt16  = aextract TypeMapper.tm_uint16
  let aasUInt32  = aextract TypeMapper.tm_uint32
  let aasUInt64  = aextract TypeMapper.tm_uint64

  let aasString : AnyTransform<string> =
    atrans <| fun o c r ->
      match o with
      | (:? string as v)        -> v                                |> good
      | (:? IFormattable as f)  -> f.ToString ("", defaultCulture)  |> good
      | _                       -> o |> string                      |> good

  // Queries

  let inline aindex idx v (t : AnyTransform<'T>) : AnyTransform<'T> =
    atrans <| fun o c r ->
      match r.Indexer o with
      | Just indexer  ->
        let (AnyContext p)        = c
        let (AnyIndexer indexer)  = indexer
        match indexer idx with
        | Just v      ->
          let ic = (AnyContextElement.Index idx)::p |> AnyContext
          invoke t v ic r
        | Nothing     ->
          result v (idx |> AnyError.IndexOutOfRange |> leaf c)
      | Nothing       ->
          result v (o |> getType |> AnyError.NotIndexable |> leaf c)

  let inline aindexz idx t =
    aindex idx (zero ()) t

  let inline amany (t : AnyTransform<'T>) : AnyTransform<'T []> =
    atrans <| fun o c r ->
      let ra = ResizeArray defaultSize
      match r.Iterator o with
      | Just iterator ->
        let et = Loops.amany t c r ra iterator empty 0
        result (ra.ToArray ()) et
      | Nothing ->
        result [||] (o |> getType |> AnyError.NotIterable |> leaf c)

  let inline amember name v (t : AnyTransform<'T>) : AnyTransform<'T> =
    atrans <| fun o c r ->
      match r.Lookup o with
      | Just lookup ->
        let (AnyContext p)      = c
        let (AnyLookup getter)  = lookup
        match getter name with
        | Just v    ->
          let ic = (AnyContextElement.Member name)::p |> AnyContext
          invoke t v ic r
        | Nothing   ->
          result v (name |> AnyError.MemberNotFound |> leaf c)
      | Nothing     ->
          result v (o |> getType |> AnyError.NotLookupable |> leaf c)

  let inline amemberz name t =
    amember name (zero ()) t

  type AnyBuilder () =
    member inline x.Bind        (t, uf) = abind       t uf
    member inline x.Combine     (t, u)  = akeepRight  t u
    member inline x.Return      v       = areturn     v
    member inline x.ReturnFrom  t       = t                     : AnyTransform<'T>
    member inline x.Zero        ()      = areturn     (zero ())

type AnyTransform<'T> with
  static member inline (>>=) (t , uf) = AnyTransform.abind      t uf
  static member inline (<*>) (tf, u)  = AnyTransform.aapply    tf u
  static member inline (|>>) (t , m)  = AnyTransform.amap       m t
  static member inline (<&>) (l , r)  = AnyTransform.aand       l r
  static member inline (<|>) (l , r)  = AnyTransform.aor        l r
  static member inline (.>>) (l , r)  = AnyTransform.akeepLeft  l r
  static member inline (>>.) (l , r)  = AnyTransform.akeepRight l r

type AnyTransformKleisli<'T, 'U> with
  static member inline (>=>) (tf, uf) = AnyTransform.akleisli tf uf

let anyTransform = AnyTransform.AnyBuilder ()
