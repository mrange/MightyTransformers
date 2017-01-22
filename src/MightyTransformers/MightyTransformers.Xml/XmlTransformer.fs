module MightyTransformers.Xml.XmlTransformer

open System.Text
open System.Xml

open MightyTransformers.Common

[<RequireQualifiedAccess>]
type XContextElement =
  | Element of  string*int
  | Named   of  string
type XContext = XContextElement list

[<RequireQualifiedAccess>]
type XError =
  | AttributeNotFound   of string
  | CheckFailed         of string
  | DescendantNotFound  of string
  | ElementNotFound     of string
  | Failure             of string
  | NoRootElement
  | Warning             of string

[<RequireQualifiedAccess>]
type XErrorTree =
  | Empty
  | Leaf  of XContext*XError
  | Fork  of XErrorTree*XErrorTree

[<RequireQualifiedAccess>]
type XElementQuery =
  | Filter  of string*(XmlElement -> bool)
  | And     of XElementQuery*XElementQuery

type XResult<'T>(v : 'T, et : XErrorTree) =
  struct
    member x.Value      = v
    member x.ErrorTree  = et
  end
type XTransform<'T> = XmlElement -> XContext -> XResult<'T>

module XQuery =

  module Details =
    let inline toLower (s : string)= 
      s.ToLowerInvariant ()

    let inline tryFindAttribute (e : XmlElement) name = 
      e.Attributes.GetNamedItem name

    let inline elementLocalName (e : XmlElement) =
      e.LocalName

    let inline elementName (e : XmlElement) =
      e.Name

  open Details

  let inline xqdescribe hnq =
    let sb            = StringBuilder 16
    let inline app s  = sb.Append (s : string) |> ignore
    let rec loop hnq =
      match hnq with
      | XElementQuery.Filter  (d, _)  -> app d
      | XElementQuery.And     (l, r)  -> loop l; app " AND "; loop r
    loop hnq
    sb.ToString ()

  let rec xqtestElement heq n =
    match heq with
    | XElementQuery.Filter  (_, f)  -> f n
    | XElementQuery.And     (l, r)  -> xqtestElement l n && xqtestElement r n

  let inline xqAnd l r =
    XElementQuery.And (l, r)

  let inline xqhasAttribute name =
    let f e = 
      tryFindAttribute e name <> null
    XElementQuery.Filter (sprintf "Expected element with attribute named '%s'" name, f)

  let inline xqhasAttributeValue name value =
    let f e = 
      match tryFindAttribute e name with
      | null  -> false
      | attr  -> attr.Value = value
    XElementQuery.Filter (sprintf "Expected element with attribute named '%s' and value '%s'" name value, f)

  let inline xqhasLocalName name =
    let f e = elementLocalName e = name
    XElementQuery.Filter (sprintf "Expected element locally named '%s'" name, f)

  let inline xqhasName name =
    let f e = elementName e = name
    XElementQuery.Filter (sprintf "Expected element named '%s'" name, f)

  let xqtrue =
    XElementQuery.Filter ("Always true", fun _ -> true)

  let inline xqfalse msg =
    XElementQuery.Filter (msg, fun _ -> false)

  module Infixes =
    let inline (<&&>) l r = xqAnd l r

module Details =
  let defaultSize     = 16
  let inline adapt f  = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f

  let inline zero ()  = LanguagePrimitives.GenericZero<_>

  let inline leaf p e = XErrorTree.Leaf (p, e)

  let inline join l r =
    match l, r with
    | XErrorTree.Empty  , _                 -> r
    | _                 , XErrorTree.Empty  -> l
    | _                 , _                 -> XErrorTree.Fork (l, r)

  let pathToString p =
    let sb = System.Text.StringBuilder defaultSize
    let inline app s = sb.Append (s : string) |> ignore
    let rec loop p =
      match p with
      | []    -> app "body"
      | h::t  ->
        loop t
        match h with
        | XContextElement.Element (e, i)  -> app (sprintf "/%s@%d" e i)
        | XContextElement.Named   n       -> app (sprintf "(%s)" n)
    loop p
    sb.ToString ()

  let collapse (et : XErrorTree) =
    let result = ResizeArray defaultSize
    let rec loop et =
      match et with
      | XErrorTree.Empty         -> ()
      | XErrorTree.Leaf  (p, e)  -> result.Add (pathToString p, e)
      | XErrorTree.Fork  (l, r)  -> loop l; loop r
    loop et
    result.ToArray ()

  let inline result  v et = XResult (v, et)

  let inline good    v    = result v XErrorTree.Empty

module XTransform =
  open FSharp.Core.Printf
  open MightyTransformers.Common
  open Details

  // Monad

  let inline xreturn v : XTransform<'T> =
    fun e p ->
      good v

  let inline xbind (t : XTransform<'T>) (uf : 'T -> XTransform<'U>) : XTransform<'U> =
    let t = adapt t
    fun e p ->
      let tr  = t.Invoke (e, p)
      let u   = uf tr.Value
      let u   = adapt u
      let ur  = u.Invoke (e, p)
      result ur.Value (join tr.ErrorTree ur.ErrorTree)

  // Kleisli

  let inline xarr f = fun v -> xreturn (f v)

  let inline xkleisli tf uf =
    fun v -> xbind (tf v) uf

  // Applicative

  let inline xpure v = xreturn v

  let inline xap (t : XTransform<'U -> 'V>) (u : XTransform<'U>) : XTransform<'V> =
    let t = adapt t
    let u = adapt u
    fun e p ->
      let tr  = t.Invoke (e, p)
      let ur  = u.Invoke (e, p)
      result (tr.Value ur.Value) (join tr.ErrorTree ur.ErrorTree)


  // Functor

  let inline xmap m (t : XTransform<'T>) : XTransform<'U> =
    let t = adapt t
    fun e p ->
      let tr = t.Invoke (e, p)
      result (m tr.Value) tr.ErrorTree

  // Combinators

  let inline xorElse (l : XTransform<'T>) (r : XTransform<'T>) : XTransform<'T> =
    let l = adapt l
    let r = adapt r
    fun e p ->
      let lr = l.Invoke (e, p)
      match lr.ErrorTree with
      | XErrorTree.Empty  -> lr
      | _                 ->
        let rr = r.Invoke (e, p)
        match rr.ErrorTree with
        | XErrorTree.Empty -> rr
        | _     ->
          result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline xkeepLeft (l : XTransform<'T>) (r : XTransform<_>) : XTransform<'T> =
    let l = adapt l
    let r = adapt r
    fun e p ->
      let lr = l.Invoke (e, p)
      let rr = r.Invoke (e, p)
      result lr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline xkeepRight (l : XTransform<_>) (r : XTransform<'T>) : XTransform<'T> =
    let l = adapt l
    let r = adapt r
    fun e p ->
      let lr = l.Invoke (e, p)
      let rr = r.Invoke (e, p)
      result rr.Value (join lr.ErrorTree rr.ErrorTree)

  let inline xpair (l : XTransform<'T>) (r : XTransform<'U>) : XTransform<'T*'U> =
    let l = adapt l
    let r = adapt r
    fun e p ->
      let lr = l.Invoke (e, p)
      let rr = r.Invoke (e, p)
      result (lr.Value, rr.Value) (join lr.ErrorTree rr.ErrorTree)

  let inline xtoOption (t : XTransform<'T>) : XTransform<'T option> =
    let t = adapt t
    fun e p ->
      let tr = t.Invoke (e, p)
      match tr.ErrorTree with
      | XErrorTree.Empty  -> good (Some tr.Value)
      | _                 -> good None

  let inline htoResult (t : XTransform<'T>) : XTransform<Result<'T, (string*XError) []>> =
    let t = adapt t
    fun e p ->
      let tr = t.Invoke (e, p)
      match tr.ErrorTree with
      | XErrorTree.Empty  -> good (Good tr.Value)
      | _                 -> good (Bad (collapse tr.ErrorTree))

  // Failures

  let inline xfailure v msg : XTransform<'T> =
    fun e p ->
      result v (msg |> XError.Failure |> leaf p)

  let inline xfailuref v fmt = kprintf (xfailure v) fmt

  let inline xwarning v msg : XTransform<'T> =
    fun e p ->
      result v (msg |> XError.Warning |> leaf p)

  let inline xwarningf v fmt = kprintf (xwarning v) fmt

  // Misc

  let inline xwithContext name (t : XTransform<'T>) : XTransform<'T> =
    let t = adapt t
    fun e p ->
      let np = (XContextElement.Named name)::p
      t.Invoke (e, np)

  let inline xdebug name (t : XTransform<'T>) : XTransform<'T> =
    let t = adapt t
    fun e p ->
      // let attrs = e.Attributes |> FsLinq.map (fun a -> a.Name, a.Value) |> FsLinq.toArray
      printfn "BEFORE  %s: %A - %A" name e.Name p
      let tr = t.Invoke (e, p)
      match tr.ErrorTree with
      | XErrorTree.Empty  -> printfn "SUCCESS %s: %A" name tr.Value
      | _                 -> printfn "FAILURE %s: %A(%A)" name tr.Value tr.ErrorTree
      tr

  let inline xrun (t : XTransform<'T>) v (doc : XmlDocument) =
    let root = doc.DocumentElement
    if root <> null then
      let t = adapt t
      let tr = t.Invoke (doc.DocumentElement, [])
      tr.Value, collapse tr.ErrorTree
    else
      v, collapse (leaf [] XError.NoRootElement)

  let inline xrunz t doc =
    xrun t (zero ()) doc

  // Extractors

  open XQuery.Details

  let inline xattribute name v : XTransform<string> =
    fun e p ->
      match tryFindAttribute e name with
      | null  -> result v (name |> XError.AttributeNotFound |> leaf p)
      | a     -> good a.Value

  let inline xattributez name =
    xattribute name ""

  let xtext : XTransform<string> =
    fun e p ->
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

  open XQuery

  // Checks 

  let xcheck (xeq : XElementQuery) : XTransform<unit> =
    fun e p ->
      if xqtestElement xeq e then
        good ()
      else
        result () (xeq |> xqdescribe |> XError.CheckFailed |> leaf p)

  // Queries

  let inline xdescendant (xeq : XElementQuery) v (t : XTransform<'T>) : XTransform<'T> =
    let t = adapt t
    fun e p ->
      let mutable r = Unchecked.defaultof<_>
      let rec loop p (ns : XmlNodeList) c i =
        if i < c then
          match ns.[i] with
          | :? XmlElement as head ->
            let ip = (XContextElement.Element (elementName head, i))::p
            if xqtestElement xeq head then
              r <- t.Invoke (head, ip)
              true
            else
              loop ip head.ChildNodes head.ChildNodes.Count 0 || loop p ns c (i + 1)
          | _ ->
            loop p ns c (i + 1) 
        else
          false
      if loop p e.ChildNodes e.ChildNodes.Count 0 then
        r
      else
        result v (xeq |> xqdescribe |> XError.DescendantNotFound |> leaf p)

  let inline xdescendantz xeq t =
    xdescendant xeq (zero ()) t

  let inline xdescendants (xeq : XElementQuery) (t : XTransform<'T>) : XTransform<'T []> =
    let t = adapt t
    fun e p ->
      let r = ResizeArray defaultSize
      let rec loop et p (ns : XmlNodeList) c i =
        if i < c then
          match ns.[i] with
          | :? XmlElement as head ->
            let ip = (XContextElement.Element (elementName head, i))::p
            if xqtestElement xeq head then
              let tr = t.Invoke (head, ip)
              match tr.ErrorTree with
              | XErrorTree.Empty  -> r.Add tr.Value
              | _                 -> ()
              loop (join et tr.ErrorTree) p ns c (i + 1)
            else
              let et = loop et ip head.ChildNodes head.ChildNodes.Count (i + 1) 
              loop et p ns c (i + 1) 
          | _ ->
            loop et p ns c (i + 1) 
        else
          et
      let et = loop XErrorTree.Empty p e.ChildNodes e.ChildNodes.Count 0
      result (r.ToArray ()) et

  let inline xelement (xeq : XElementQuery) v (t : XTransform<'T>) : XTransform<'T> =
    let t = adapt t
    fun e p ->
      let mutable r = Unchecked.defaultof<_>
      let rec loop p (ns : XmlNodeList) c i =
        if i < c then
          match ns.[i] with
          | :? XmlElement as head ->
            let ip = (XContextElement.Element (elementName head, i))::p
            if xqtestElement xeq head then
              r <- t.Invoke (head, ip)
              true
            else
              loop p ns c (i + 1)
          | _ ->
            loop p ns c (i + 1) 
        else
          false
      if loop p e.ChildNodes e.ChildNodes.Count 0 then
        r
      else
        result v (xeq |> xqdescribe |> XError.ElementNotFound |> leaf p)

  let inline xelementz xeq t =
    xelement xeq (zero ()) t

  let (*inline*) xelements (xeq : XElementQuery) (t : XTransform<'T>) : XTransform<'T []> =
    let t = adapt t
    fun e p ->
      let r = ResizeArray defaultSize
      let rec loop et p (ns : XmlNodeList) c i =
        if i < c then
          match ns.[i] with
          | :? XmlElement as head ->
            let ip = (XContextElement.Element (elementName head, i))::p
            if xqtestElement xeq head then
              let tr = t.Invoke (head, ip)
              match tr.ErrorTree with
              | XErrorTree.Empty  -> r.Add tr.Value
              | _                 -> ()
              loop (join et tr.ErrorTree) p ns c (i + 1)
            else
              loop et p ns c (i + 1) 
          | _ ->
            loop et p ns c (i + 1) 
        else
          et
      let et = loop XErrorTree.Empty p e.ChildNodes e.ChildNodes.Count 0
      result (r.ToArray ()) et

  type XBuilder() =
    member inline x.Bind        (t, uf) = xbind t uf
    member inline x.Return      v       = xreturn v
    member inline x.ReturnFrom  t       = t : XTransform<'T>

  module Infixes =
    let inline (>>=)  t uf  = xbind      t uf
    let inline (>=>) tf uf  = xkleisli  tf uf
    let inline (<*>) tf u   = xap       tf u
    let inline (|>>)  t m   = xmap       m t
    let inline (<|>)  l r   = xorElse    l r
    let inline (.>>.) l r   = xpair      l r
    let inline (.>>)  l r   = xkeepLeft  l r
    let inline (>>.)  l r   = xkeepRight l r

let xtransform = XTransform.XBuilder ()
