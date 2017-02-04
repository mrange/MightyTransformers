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

module MightyTransformers.Xml.XmlTransformer

open System.Text
open System.Xml

[<RequireQualifiedAccess>]
type XName =
  | Global    of string*string
  | Local     of string
  | Qualified of string*string*string

  override x.ToString () =
    match x with
    | Global    (localName, namespaceUri)   ->
      if namespaceUri.Length = 0 then
        localName
      else
        sprintf "{%s#%s}" namespaceUri localName
    | Local     (localName)                 -> localName
    | Qualified (localName, prefix, qname)  -> qname

module XNames =
  let inline xnglobal     localName namespaceUri  = XName.Global    (localName, namespaceUri)
  let inline xnlocal      localName               = XName.Local     (localName)
  let inline xnqualified  localName prefix        = XName.Qualified (localName, prefix, if prefix.Length = 0 then localName else prefix + ":" + localName)

[<RequireQualifiedAccess>]
type XContextElement =
  | Element of  XName*int
  | Named   of  string
type XContext = XContextElement list

[<RequireQualifiedAccess>]
type XError =
  | AttributeNotFound   of XName
  | CheckFailed         of string
  | DescendantNotFound  of string
  | ElementNotFound     of string
  | Message             of string
  | NoParentElement
  | NoRootElement

[<RequireQualifiedAccess>]
type XErrorTree =
  | Empty
  | Leaf      of XContext*XError
  | Suppress  of XErrorTree
  | Fork      of XErrorTree*XErrorTree

[<RequireQualifiedAccess>]
type XElementQuery =
  | Filter  of string*(XmlElement -> bool)
  | And     of XElementQuery*XElementQuery

type XResult<'T>(v : 'T, et : XErrorTree) =
  struct
    member x.Value      = v
    member x.ErrorTree  = et
  end

type XErrorItem =
  {
    IsSuppressed  : bool
    Path          : string
    Error         : XError
  }
  static member New i p e : XErrorItem = { IsSuppressed = i; Path = p; Error = e }

[<NoEquality>]
[<NoComparison>]
type XTransform<'T> (f : OptimizedClosures.FSharpFunc<XmlElement, XContext, XResult<'T>>) = 
  struct
    member x.Invoke (j, p) = f.Invoke (j, p)
  end


module XElementQueries =

  module Details =
    module Loops =
      let rec tryFindAttribute_Localname localName (xac : XmlAttributeCollection) c i =
        if i < c then
          let xa = xac.[i : int]
          if xa.LocalName = localName then
            xa
          else
            tryFindAttribute_Localname localName xac c (i + 1)
        else
          null

      let rec xqdescribe (sb : StringBuilder) hnq =
        match hnq with
        | XElementQuery.Filter  (d, _)  -> sb.Append d |> ignore
        | XElementQuery.And     (l, r)  -> xqdescribe sb l; sb.Append " AND " |> ignore; xqdescribe sb r

    let inline toLower (s : string)=
      s.ToLowerInvariant ()

    let inline tryFindAttribute name (e : XmlElement) =
      match name with
      | XName.Global    (localName, namespaceUri) -> e.Attributes.GetNamedItem (localName, namespaceUri) :?> XmlAttribute
      | XName.Local     (localName)               -> Loops.tryFindAttribute_Localname localName e.Attributes e.Attributes.Count 0
      | XName.Qualified (localName, prefix, qname)-> e.Attributes.GetNamedItem (qname) :?> XmlAttribute


    let inline hasElementName name (e : XmlElement) =
      match name with
      | XName.Global    (localName, namespaceUri) -> e.LocalName = localName && e.NamespaceURI = namespaceUri
      | XName.Local     (localName)               -> e.LocalName = localName
      | XName.Qualified (localName, prefix, qname)-> e.Name      = qname

    let inline toString (x : #obj) = x.ToString ()

  open Details

  let inline xqdescribe hnq =
    let sb            = StringBuilder 16
    Loops.xqdescribe sb hnq
    sb.ToString ()

  let rec xqtestElement heq n =
    match heq with
    | XElementQuery.Filter  (_, f)  -> f n
    | XElementQuery.And     (l, r)  -> xqtestElement l n && xqtestElement r n

  let inline xqAnd l r =
    XElementQuery.And (l, r)

  let inline xqhasAttribute name =
    let f e =
      tryFindAttribute name e <> null
    XElementQuery.Filter (sprintf "Expected element with attribute named '%s'" (toString name), f)

  let inline xqhasAttributeValue name value =
    let f e =
      match tryFindAttribute name e with
      | null  -> false
      | attr  -> attr.Value = value
    XElementQuery.Filter (sprintf "Expected element with attribute named '%s' and value '%s'" (toString name) value, f)

  let inline xqhasName name =
    let f e =
      hasElementName name e
    XElementQuery.Filter (sprintf "Expected element named '%s'" (toString name), f)

  let xqtrue =
    XElementQuery.Filter ("Always true", fun _ -> true)

  let inline xqfalse msg =
    XElementQuery.Filter (msg, fun _ -> false)

  module Infixes =
    let inline (<&&>) l r = xqAnd l r

module XTransform =
  open FSharp.Core.Printf

  module Details =
    open XElementQueries.Details

    let defaultSize     = 16

    let inline xtrans f = XTransform(OptimizedClosures.FSharpFunc<_, _, _>.Adapt f)

    let inline zero ()  = LanguagePrimitives.GenericZero<_>

    let inline leaf p e = XErrorTree.Leaf (p, e)

    let empty           = XErrorTree.Empty

    let inline isGood e=
      match e with
      | XErrorTree.Empty
      | XErrorTree.Suppress _ -> true
      | _                     -> false

    let inline supp e   =
      match e with
      | XErrorTree.Empty  -> e
      | _                 -> XErrorTree.Suppress e

    let inline join l r =
      match l, r with
      | XErrorTree.Empty      , _                     -> r
      | _                     , XErrorTree.Empty      -> l
      | XErrorTree.Suppress l , XErrorTree.Suppress r -> XErrorTree.Fork (l, r) |> XErrorTree.Suppress
      | _                     , _                     -> XErrorTree.Fork (l, r)

    let inline invoke (t : XTransform<'T>) e p = t.Invoke (e, p)

    let inline elementName (e : XmlElement) =
      XNames.xnqualified e.LocalName e.Prefix

    let pathToString p =
      // Verified that call to private pathToString don't do "funny" stuff
      let rec pathToString (sb : StringBuilder) p =
        match p with
        | []    -> sb.Append "." |> ignore
        | h::t  ->
          pathToString sb t
          match h with
          | XContextElement.Element (e, i)  -> sb.Append (sprintf "/%s'%d" (toString e) i)  |> ignore
          | XContextElement.Named   n       -> sb.Append (sprintf "(%s)" n)                 |> ignore
      let sb = System.Text.StringBuilder defaultSize
      pathToString sb p
      sb.ToString ()

    let collapse (et : XErrorTree) =
      // Verified that call to private collapse don't do "funny" stuff
      let rec collapse suppress (result : ResizeArray<_>) et =
        match et with
        | XErrorTree.Empty            -> ()
        | XErrorTree.Leaf     (p, e)  -> XErrorItem.New suppress (pathToString p) e |> result.Add
        | XErrorTree.Suppress e       -> collapse true result e
        | XErrorTree.Fork     (l, r)  -> collapse suppress result l; collapse suppress result r
      let result = ResizeArray defaultSize
      collapse false result et
      result.ToArray ()

    let inline result  v et = XResult (v, et)

    let inline good    v    = result v empty

    module Loops =
      open XElementQueries

      let rec xdescendant xeq r t p (ns : XmlNodeList) c i =
        if i < c then
          match ns.[i] with
          | :? XmlElement as head ->
            let ip = (XContextElement.Element (elementName head, i))::p
            if xqtestElement xeq head then
              r := invoke t head ip
              true
            else
              xdescendant xeq r t ip head.ChildNodes head.ChildNodes.Count 0 || xdescendant xeq r t p ns c (i + 1)
          | _ ->
            xdescendant xeq r t p ns c (i + 1)
        else
          false

      let rec xdescendants xeq r t et p (ns : XmlNodeList) c i =
        if i < c then
          match ns.[i] with
          | :? XmlElement as head ->
            let ip = (XContextElement.Element (elementName head, i))::p
            if xqtestElement xeq head then
              let tr = invoke t head ip
              if isGood tr.ErrorTree then
                (r : ResizeArray<_>).Add tr.Value
              xdescendants xeq r t (join et tr.ErrorTree) p ns c (i + 1)
            else
              let et = xdescendants xeq r t et ip head.ChildNodes head.ChildNodes.Count (i + 1)
              xdescendants xeq r t et p ns c (i + 1)
          | _ ->
            xdescendants xeq r t et p ns c (i + 1)
        else
          et

      let rec xelement xeq r t p (ns : XmlNodeList) c i =
        if i < c then
          match ns.[i] with
          | :? XmlElement as head ->
            let ip = (XContextElement.Element (elementName head, i))::p
            if xqtestElement xeq head then
              r := invoke t head ip
              true
            else
              xelement xeq r t p ns c (i + 1)
          | _ ->
            xelement xeq r t p ns c (i + 1)
        else
          false

      let rec xelements xeq r t et p (ns : XmlNodeList) c i =
        if i < c then
          match ns.[i] with
          | :? XmlElement as head ->
            let ip = (XContextElement.Element (elementName head, i))::p
            if xqtestElement xeq head then
              let tr = invoke t head ip
              if isGood tr.ErrorTree then
                (r : ResizeArray<_>).Add tr.Value
              xelements xeq r t (join et tr.ErrorTree) p ns c (i + 1)
            else
              xelements xeq r t et p ns c (i + 1)
          | _ ->
            xelements xeq r t et p ns c (i + 1)
        else
          et

  open Details

  // Monad

  let inline xreturn v : XTransform<'T> =
    xtrans <|fun e p ->
      good v

  let inline xbind (t : XTransform<'T>) (uf : 'T -> XTransform<'U>) : XTransform<'U> =
    xtrans <| fun e p ->
      let tr  = invoke t e p
      let u   = uf tr.Value
      let ur  = invoke u e p
      result ur.Value (join tr.ErrorTree ur.ErrorTree)

  // Kleisli

  let inline xarr f = fun v -> xreturn (f v)

  let inline xkleisli tf uf =
    fun v -> xbind (tf v) uf

  // Applicative

  let inline xpure v = xreturn v

  let inline xapply (t : XTransform<'U -> 'V>) (u : XTransform<'U>) : XTransform<'V> =
    xtrans <| fun e p ->
      let tr  = invoke t e p
      let ur  = invoke u e p
      result (tr.Value ur.Value) (join tr.ErrorTree ur.ErrorTree)

  // Functor

  let inline xmap m (t : XTransform<'T>) : XTransform<'U> =
    xtrans <| fun e p ->
      let tr = invoke t e p
      result (m tr.Value) tr.ErrorTree

  // Combinators

  let inline xorElse (l : XTransform<'T>) (r : XTransform<'T>) : XTransform<'T> =
    xtrans <| fun e p ->
      let lr = invoke l e p
      if isGood lr.ErrorTree then
        lr
      else
        let rr = invoke r e p
        if isGood rr.ErrorTree then
          rr
        else
          result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline xkeepLeft (l : XTransform<'T>) (r : XTransform<_>) : XTransform<'T> =
    xtrans <| fun e p ->
      let lr = invoke l e p
      let rr = invoke r e p
      result lr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline xkeepRight (l : XTransform<_>) (r : XTransform<'T>) : XTransform<'T> =
    xtrans <| fun e p ->
      let lr = invoke l e p
      let rr = invoke r e p
      result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline xpair (l : XTransform<'T>) (r : XTransform<'U>) : XTransform<'T*'U> =
    xtrans <| fun e p ->
      let lr = invoke l e p
      let rr = invoke r e p
      result (lr.Value, rr.Value) (join lr.ErrorTree rr.ErrorTree)

  let inline xsuppress (t : XTransform<'T>) : XTransform<'T> =
    xtrans <| fun e p ->
      let tr = invoke t e p
      let e  =
        if isGood tr.ErrorTree then
          tr.ErrorTree
        else
          tr.ErrorTree |> supp
      result tr.Value e

  let inline xtoOption (t : XTransform<'T>) : XTransform<'T option> =
    xtrans <| fun e p ->
      let tr = invoke t e p
      if isGood tr.ErrorTree then
        good (Some tr.Value)
      else
        good None

#if FSHARP_41
  let inline xtoResult (t : XTransform<'T>) : XTransform<Result<'T, XErrorItem []>> =
    xtrans <| fun e p ->
      let tr = invoke t e p
      if isGood tr.ErrorTree then
        good (Good tr.Value)
      else
        good (Bad (collapse tr.ErrorTree))
#endif

  let inline xunpack (ok : 'T -> XTransform<'U>) (bad : XErrorItem [] -> XTransform<'U>) (t : XTransform<'T>) =
    xtrans <| fun e p ->
      let tr = invoke t e p
      if isGood tr.ErrorTree then
        let tok = ok tr.Value
        invoke tok e p
      else
        let tbad = bad (collapse tr.ErrorTree)
        invoke tbad e p

  // Failures

  let inline xfailure v msg : XTransform<'T> =
    xtrans <| fun e p ->
      result v (msg |> XError.Message |> leaf p)

  let inline xfailuref v fmt = kprintf (xfailure v) fmt

  let inline xwarning v msg : XTransform<'T> =
    xtrans <| fun e p ->
      result v (msg |> XError.Message |> leaf p |> supp)

  let inline xwarningf v fmt = kprintf (xwarning v) fmt

  // Misc

  let inline xwithContext name (t : XTransform<'T>) : XTransform<'T> =
    xtrans <| fun e p ->
      let np = (XContextElement.Named name)::p
      invoke t e np

  let inline xdebug name (t : XTransform<'T>) : XTransform<'T> =
    xtrans <| fun e p ->
      // let attrs = e.Attributes |> FsLinq.map (fun a -> a.Name, a.Value) |> FsLinq.toArray
      printfn "BEFORE  %s: %A - %A" name e.Name p
      let tr = invoke t e p
      if isGood tr.ErrorTree then
        printfn "SUCCESS %s: %A(%A)" name tr.Value tr.ErrorTree
      else
        printfn "FAILURE %s: %A(%A)" name tr.Value tr.ErrorTree
      tr

  let inline xrun (t : XTransform<'T>) v (root : XmlElement) : 'T*XErrorItem [] =
    if root <> null then
      let tr = invoke t root []
      tr.Value, collapse tr.ErrorTree
    else
      v, collapse (leaf [] XError.NoRootElement)

  let inline xrunz t root =
    xrun t (zero ()) root

  // Extractors

  open XElementQueries.Details

  let inline xattribute name v : XTransform<string> =
    xtrans <| fun e p ->
      match tryFindAttribute name e with
      | null  -> result v (name |> XError.AttributeNotFound |> leaf p)
      | a     -> good a.Value

  let inline xattributez name =
    xattribute name ""

  let xtext : XTransform<string> =
    xtrans <| fun e p ->
      good e.Value

  // Mappers

  let xtoInt32 =
    fun t ->
      xbind t
        (fun v ->
          match System.Int32.TryParse v with
          | true  , i  -> xreturn i
          | false , _  -> xfailuref 0 "Not a valid integer (%s)" v
        )

  let xtrim t =
    xmap (fun (s : string) -> s.Trim ()) t

  open XElementQueries

  // Checks

  let xcheck (xeq : XElementQuery) : XTransform<unit> =
    xtrans <| fun e p ->
      if xqtestElement xeq e then
        good ()
      else
        result () (xeq |> xqdescribe |> XError.CheckFailed |> leaf p)

  // Queries

  let inline xdescendant (xeq : XElementQuery) v (t : XTransform<'T>) : XTransform<'T> =
    xtrans <| fun e p ->
      let r = ref Unchecked.defaultof<_>
      if Loops.xdescendant xeq r t p e.ChildNodes e.ChildNodes.Count 0 then
        !r
      else
        result v (xeq |> xqdescribe |> XError.DescendantNotFound |> leaf p)

  let inline xdescendantz xeq t =
    xdescendant xeq (zero ()) t

  let inline xdescendants (xeq : XElementQuery) (t : XTransform<'T>) : XTransform<'T []> =
    xtrans <| fun e p ->
      let r = ResizeArray defaultSize
      let et = Loops.xdescendants xeq r t empty p e.ChildNodes e.ChildNodes.Count 0
      result (r.ToArray ()) et

  let inline xelement (xeq : XElementQuery) v (t : XTransform<'T>) : XTransform<'T> =
    xtrans <| fun e p ->
      let r = ref Unchecked.defaultof<_>
      if Loops.xelement xeq r t p e.ChildNodes e.ChildNodes.Count 0 then
        !r
      else
        result v (xeq |> xqdescribe |> XError.ElementNotFound |> leaf p)

  let inline xelementz xeq t =
    xelement xeq (zero ()) t

  let inline xelements (xeq : XElementQuery) (t : XTransform<'T>) : XTransform<'T []> =
    xtrans <| fun e p ->
      let r = ResizeArray defaultSize
      let et = Loops.xelements xeq r t empty p e.ChildNodes e.ChildNodes.Count 0
      result (r.ToArray ()) et

  let inline xparent v (t : XTransform<'T>) : XTransform<'T> =
    xtrans <| fun e p ->
      match p, e.ParentNode with
      | _::p, (:? XmlElement as e) ->
        invoke t e p
      | _ ->
        result v (XError.NoParentElement |> leaf p)

  type XBuilder() =
    member inline x.Bind        (t, uf) = xbind t uf
    member inline x.Return      v       = xreturn v
    member inline x.ReturnFrom  t       = t : XTransform<'T>

  module Infixes =
    let inline (>>=)  t uf  = xbind      t uf
    let inline (>=>) tf uf  = xkleisli  tf uf
    let inline (<*>) tf u   = xapply    tf u
    let inline (|>>)  t m   = xmap       m t
    let inline (<|>)  l r   = xorElse    l r
    let inline (.>>.) l r   = xpair      l r
    let inline (.>>)  l r   = xkeepLeft  l r
    let inline (>>.)  l r   = xkeepRight l r

let xtransform = XTransform.XBuilder ()
