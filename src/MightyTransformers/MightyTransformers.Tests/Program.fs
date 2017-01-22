module Common =
  open FSharp.Core.Printf

  open System
  open System.Text

  let cline (cc : ConsoleColor) (prelude : string) (text : string) : unit =
    let b = Console.ForegroundColor
    try
      Console.ForegroundColor <- cc
      Console.Write     prelude
      Console.WriteLine text
    finally
      Console.ForegroundColor <- b

  let mutable errorTraceCount = 0

  let success   msg = cline ConsoleColor.Green  "SUCCESS: " msg
  let hilight   msg = cline ConsoleColor.White  "HILIGHT: " msg
  let info      msg = cline ConsoleColor.Gray   "INFO   : " msg
  let warning   msg = cline ConsoleColor.Yellow "WARNING: " msg
  let error     msg =
    System.Threading.Interlocked.Increment &errorTraceCount |> ignore
    cline ConsoleColor.Red "ERROR  : " msg

  let successf  fmt = kprintf success fmt
  let hilightf  fmt = kprintf hilight fmt
  let infof     fmt = kprintf info    fmt
  let warningf  fmt = kprintf warning fmt
  let errorf    fmt = kprintf error   fmt

  let dispose (d : #IDisposable) =
    if d <> null then
      try
        d.Dispose ()
      with
      | e ->
        error "Failed to dispose object"

module XmlTransformerTest =
  open Common
  open MightyTransformers.Xml.XmlTransformer
  open MightyTransformers.Xml.XmlTransformer.XElementQueries
  open MightyTransformers.Xml.XmlTransformer.XElementQueries.Infixes
  open MightyTransformers.Xml.XmlTransformer.XTransform
  open MightyTransformers.Xml.XmlTransformer.XTransform.Infixes
  open System.Xml

  type Work =
    | Book        of string
    | Manuscript  of string

  type Author =
    {
      FirstName   : string
      LastName    : string
      YearOfBirth : int option
      Works       : Work []
    }

    static member New firstName lastName yearOfBirth works : Author =
      {
        FirstName   = firstName
        LastName    = lastName
        YearOfBirth = yearOfBirth
        Works       = works
      }

    static member Empty = Author.New "" "" None [||]

  let testAuthorsTransform () =
    let xml = """<?xml version="1.0" encoding="utf-8" ?>
<authors>
  <author name="Ludwig" surname="Wittgenstein">
    <book title="Tractatus Logico-Philosophicus" />
    <book title="Philosophical Investigations" />
    <manuscript title="Notes on Logic" />
  </author>
  <author name="Rene" surname="Descartes" birth="1596">
    <book title="Discourse on Method and the Meditations" />
    <book title="Meditations and Other Metaphysical Writings" />
    <notes title="Some unfinished notes" />
  </author>
</authors>
"""
//    <notes title="Some unfinished notes" />

    let xnauthors     = XNames.xnlocal "authors"
    let xnbirth       = XNames.xnlocal "birth"
    let xnbook        = XNames.xnlocal "book"
    let xnmanuscript  = XNames.xnlocal "manuscript"
    let xnname        = XNames.xnlocal "name"
    let xnsurname     = XNames.xnlocal "surname"
    let xntitle       = XNames.xnlocal "title"

    let xbook       = xcheck (xqhasName xnbook)       >>. (xattributez xntitle |>> Book)
    let xmanuscript = xcheck (xqhasName xnmanuscript) >>. (xattributez xntitle |>> Manuscript)
    let xwork       = xbook <|> xmanuscript
    let xauthor     =
      xpure Author.New
      <*> (xattributez xnname)
      <*> (xattributez xnsurname)
      <*> (xattributez xnbirth |> xtoInt32 |> xtoOption)
      <*> (xelements xqtrue xwork)
    let xauthors    = xcheck (xqhasName xnauthors) >>. xelements xqtrue xauthor

    let xdoc = XmlDocument ()
    xdoc.LoadXml xml
    let xres = xrun xauthors [||] xdoc

    infof "%A" xres

  let run () =
    testAuthorsTransform ()

open Common
open System

[<EntryPoint>]
let main argv =
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

    XmlTransformerTest.run ()

    if Common.errorTraceCount > 0 then
      errorf "Detected %d error(s)" Common.errorTraceCount
      998
    else
      success "No errors detected"
      0

  with
  | e ->
    errorf "Caught exception: %s" e.Message
    999
