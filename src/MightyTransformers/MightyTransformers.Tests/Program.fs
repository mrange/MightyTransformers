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

module JsonTransformerTest =
  open Common
  open MightyTransformers.Json.JsonTransformer
  open MightyTransformers.Json.JsonTransformer.JTransform
  open MightyTransformers.Json.JsonTransformer.JTransform.Infixes

  open MiniJson.JsonModule

  type Work =
    | Book        of string
    | Manuscript  of string

  type Author =
    {
      FirstName   : string
      LastName    : string
      YearOfBirth : float option
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
    let json = """
[
    {
        "name"    : "Ludwig"
      , "surname" : "Wittgenstein"
      , "works"   : [
          {
              "kind"    : "book"
            , "title"   : "Tractatus Logico-Philosophicus"
          }
        , {
              "kind"    : "book"
            , "title"   : "Philosophical Investigations"
          }
        , {
              "kind"    : "manuscript"
            , "title"   : "Notes on Logic"
          }
      ]
    }
  , {
        "name"    : "Rene"
      , "surname" : "Descartes"
      , "birth"   : 1596
      , "works"   : [
          {
              "kind"    : "book"
            , "title"   : "Discourse on Method and the Meditations"
          }
        , {
              "kind"    : "book"
            , "title"   : "Meditations and Other Metaphysical Writings"
          }
        , {
              "kind"    : "notes"
            , "title"   : "Some unfinished notes"
          }
      ]
    }
]
"""

    let jkind kind =
      jmember "kind" ()
        (jasString >>= fun v -> if v = kind then jreturn () else jfailuref () "Expected kind '%s' but found '%s'" kind v)

    let jstr k      = jmember k "" jasString
    let jtitle      = jstr "title"
    let jbook       = jkind "book"        >>. jtitle |>> Book
    let jmanuscript = jkind "manuscript"  >>. jtitle |>> Manuscript
    let jwork       = jbook <|> jmanuscript
    let jworks      = jmember "works" [||] (jmany jwork)
    let jauthor     =
      jpair (jstr "name") (jstr "surname")
      >>= fun (name, surname) ->
        jpure (Author.New name surname)
        <*> (jmemberz "birth" jasFloat |> jtoOption)
        <*> jworks
        |> jwithContext (sprintf "%s %s" name surname)
    let jauthors    = jmany jauthor

    match parse true json with
    | ParseResult.Success json       ->
      let jres = jrun jauthors json
      infof "%A" jres
    | ParseResult.Failure (msg, pos) ->
      errorf "Failed to parse json: %s" msg

    ()

  let run () =
    testAuthorsTransform ()

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

    let xn n          = XNames.xnlocal n

    let xnauthors     = xn "authors"
    let xnbirth       = xn "birth"
    let xnbook        = xn "book"
    let xnmanuscript  = xn "manuscript"
    let xnname        = xn "name"
    let xnsurname     = xn "surname"
    let xntitle       = xn "title"

    let xbook         = xcheck (xqhasName xnbook)       >>. (xattributez xntitle |>> Book)
    let xmanuscript   = xcheck (xqhasName xnmanuscript) >>. (xattributez xntitle |>> Manuscript)
    let xwork         = xbook <|> xmanuscript
    let xworks        = (xelements xqtrue xwork)
    let xauthor       =
      xpure Author.New
      <*> (xattributez xnname)
      <*> (xattributez xnsurname)
      <*> (xattributez xnbirth |> xtoInt32 |> xtoOption)
      <*> xworks
    let xauthor       =
      xpair (xattributez xnname) (xattributez xnsurname)
      >>= fun (name, surname) ->
        xpure (Author.New name surname)
        <*> (xattributez xnbirth |> xtoInt32 |> xtoOption)
        <*> xworks
        |> xwithContext (sprintf "%s %s" name surname)
    let xauthors    = xcheck (xqhasName xnauthors) >>. xelements xqtrue xauthor

    let xdoc = XmlDocument ()
    xdoc.LoadXml xml
    let xres = xrun xauthors [||] xdoc.DocumentElement

    infof "%A" xres

  let run () =
    testAuthorsTransform ()

open Common
open System

[<EntryPoint>]
let main argv =
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

    JsonTransformerTest.run ()
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
