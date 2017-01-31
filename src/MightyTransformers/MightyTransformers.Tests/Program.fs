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

  module Functionality =
    open MightyTransformers.Common

    let expect e a =
      if e <> a then
        errorf "%A <> %A" e a

    let jok  v    = jreturn  v
    let jok0      = jok 0
    let jok1      = jok 1
    let jok2      = jok 2
    let jerr v c  = jfailure v "Error" |> jwithContext c
    let jerr0     = jerr 0 "0"
    let jerr1     = jerr 0 "1"
    let jerr2     = jerr 0 "2"
    let jfok      = jarr ((+) 1)
    let jferr     = fun v -> jerr 0 (v.ToString ())
    let jres0     = "root(0)", JError.Failure "Error"
    let jres1     = "root(1)", JError.Failure "Error"
    let jres2     = "root(2)", JError.Failure "Error"

    module Primitives =
      let jexpect jv jerrs j =
        let a = jrun j JsonNull
        expect (jv, jerrs) a

      let test_jreturn () =
        info "test_jreturn"
        jexpect 1 [||] <| jreturn 1

      let test_jbind () =
        info "test_jbind"
        jexpect 2 [||]              <| jbind jok1  jfok
        jexpect 0 [|jres1|]         <| jbind jok1  jferr
        jexpect 1 [|jres1|]         <| jbind jerr1 jfok
        jexpect 0 [|jres1; jres0|]  <| jbind jerr1 jferr

      let test_jarr () =
        info "test_jarr"
        jexpect 2 [||] <| jfok 1

      let test_jkleisli () =
        info "test_jkleisli"
        jexpect 3 [||]              <| jkleisli jfok  jfok  1
        jexpect 0 [|jres2|]         <| jkleisli jfok  jferr 1
        jexpect 1 [|jres1|]         <| jkleisli jferr jfok  1
        jexpect 0 [|jres1; jres0|]  <| jkleisli jferr jferr 1

      let test_jpure () =
        info "test_jpure"
        jexpect 1 [||] <| jpure 1

      let test_japply () =
        info "test_japply"
        let f = (+) 1
        jexpect 3 [||]              <| japply (jpure f      ) jok2
        jexpect 1 [|jres2|]         <| japply (jpure f      ) jerr2
        jexpect 2 [|jres1|]         <| japply (jerr  id "1" ) jok2
        jexpect 0 [|jres1; jres2|]  <| japply (jerr  id "1" ) jerr2

      let test_jmap () =
        info "test_jmap"
        let f = (+) 1
        jexpect 2 [||]              <| jmap f jok1
        jexpect 1 [|jres1|]         <| jmap f jerr1

      let test_jorElse () =
        info "test_jorElse"
        jexpect 1 [||]              <| jorElse jok1   jok2
        jexpect 1 [||]              <| jorElse jok1   jerr2
        jexpect 2 [||]              <| jorElse jerr1  jok2
        jexpect 0 [|jres1; jres2|]  <| jorElse jerr1  jerr2

      let test_jkeepLeft () =
        info "test_jkeepLeft"
        jexpect 1 [||]              <| jkeepLeft jok1  jok2
        jexpect 1 [|jres2|]         <| jkeepLeft jok1  jerr2
        jexpect 0 [|jres1|]         <| jkeepLeft jerr1 jok2
        jexpect 0 [|jres1; jres2|]  <| jkeepLeft jerr1 jerr2

      let test_jkeepRight () =
        info "test_jkeepRight"
        jexpect 2 [||]              <| jkeepRight jok1  jok2
        jexpect 0 [|jres2|]         <| jkeepRight jok1  jerr2
        jexpect 2 [|jres1|]         <| jkeepRight jerr1 jok2
        jexpect 0 [|jres1; jres2|]  <| jkeepRight jerr1 jerr2

      let test_jpair () =
        info "test_jpair"
        jexpect (1,2) [||]              <| jpair jok1  jok2
        jexpect (1,0) [|jres2|]         <| jpair jok1  jerr2
        jexpect (0,2) [|jres1|]         <| jpair jerr1 jok2
        jexpect (0,0) [|jres1; jres2|]  <| jpair jerr1 jerr2

      let test_jtoOption () =
        info "test_jtoOption"
        jexpect (Some 1)  [||]          <| jtoOption jok1
        jexpect None      [||]          <| jtoOption jerr1

      let test_jtoResult () =
        info "test_jtoResult"
        jexpect (Good 1)        [||]    <| jtoResult jok1
        jexpect (Bad [|jres1|]) [||]    <| jtoResult jerr1

      let test_jfailure () =
        info "test_jfailure"
        jexpect 0 [|"root", JError.Failure "Hello"|] <| jfailure 0 "Hello"
        jexpect 0 [|"root", JError.Failure "There"|] <| jfailure 0 "There"

      let test_jfailuref () =
        info "test_jfailuref"
        jexpect 0 [|"root", JError.Failure "Hello there"|] <| jfailuref 0 "Hello %s" "there"

      let test_jwarning () =
        info "test_jwarning"
        jexpect 0 [|"root", JError.Warning "Hello"|] <| jwarning 0 "Hello"
        jexpect 0 [|"root", JError.Warning "There"|] <| jwarning 0 "There"

      let test_jwarningf () =
        info "test_jwarningf"
        jexpect 0 [|"root", JError.Warning "Hello there"|] <| jwarningf 0 "Hello %s" "there"

      let test_jwithContext () =
        info "test_jwithContext"
        jexpect 1 [||]                                          <| jwithContext "Hello" jok1
        jexpect 0 [|"root(Hello)(1)", JError.Failure "Error"|]  <| jwithContext "Hello" jerr1

      let test_jdebug () =
        info "test_jdebug"
        jexpect 1 [||]              <| jdebug "Hello" jok1
        jexpect 0 [|jres1|]         <| jdebug "There" jerr1

      let run () =
        test_jreturn      ()
        test_jbind        ()
        test_jarr         ()
        test_jkleisli     ()
        test_jpure        ()
        test_japply       ()
        test_jmap         ()
        test_jorElse      ()
        test_jkeepLeft    ()
        test_jkeepRight   ()
        test_jpair        ()
        test_jtoOption    ()
        test_jtoResult    ()
        test_jfailure     ()
        test_jfailuref    ()
        test_jwarning     ()
        test_jwarningf    ()
        test_jwithContext ()
        test_jdebug       ()

    // TODO: test_jrun

    module Extractors =
      let jexpect jv jerrs t json =
        expect (jv, jerrs) <| jrun t json

      let jnull   = JsonNull
      let jfalse  = JsonBoolean false
      let jtrue   = JsonBoolean true
      let jfloat0 = JsonNumber  0.0
      let jfloat1 = JsonNumber  1.0
      let jhello  = JsonString  "Hello"
      let jws     = JsonString  ""
      let jobj    = JsonObject [|"hello", JsonString "there"|]
      let jarr    = JsonArray  [|JsonBoolean true; JsonString "hello"|]
      let jerr e  = "root", e

      let test_jisNull () =
        info "test_jisNull"
        jexpect true  [||] jisNull <| jnull
        jexpect false [||] jisNull <| jfalse

      let test_jbool () =
        info "test_jbool"
        jexpect false [||]                      jbool <| jfalse
        jexpect true  [||]                      jbool <| jtrue
        jexpect false [|jerr JError.NotABool|]  jbool <| jnull

      let test_jfloat () =
        info "test_jfloat"
        jexpect 0.  [||]                      jfloat <| jfloat0
        jexpect 1.  [||]                      jfloat <| jfloat1
        jexpect 0.  [|jerr JError.NotAFloat|] jfloat <| jnull

      let test_jstring () =
        info "test_jstring"
        jexpect ""      [||]                        jstring <| jws
        jexpect "Hello" [||]                        jstring <| jhello
        jexpect ""      [|jerr JError.NotAString|]  jstring <| jnull

      let test_jvalue () =
        info "test_jvalue"
        jexpect jws     [||]  jvalue <| jws
        jexpect jhello  [||]  jvalue <| jhello
        jexpect jnull   [||]  jvalue <| jnull

      let test_jindex () =
        info "test_jindex"
        let j i = jindex i "" jstring
        let j0  = j 0
        let j1  = j 1
        let j2  = j 2
        let jm1 = j -1
        jexpect ""      [|jerr (JError.IndexOutOfRange -1)|]  jm1<| jobj
        jexpect ""      [|jerr (JError.IndexOutOfRange -1)|]  jm1<| jarr
        jexpect "there" [||]                                  j0 <| jobj
        jexpect ""      [|"root.[0]", JError.NotAString|]     j0 <| jarr
        jexpect ""      [|jerr (JError.IndexOutOfRange 1)|]   j1 <| jobj
        jexpect "hello" [||]                                  j1 <| jarr
        jexpect ""      [|jerr (JError.IndexOutOfRange 2)|]   j2 <| jobj
        jexpect ""      [|jerr (JError.IndexOutOfRange 2)|]   j2 <| jarr
        jexpect ""      [|jerr JError.NotAnArrayOrObject|]    j0 <| jws

      let test_jmember () =
        info "test_jmember"
        let jhello = jmember "hello" "" jstring
        let jthere = jmember "there" "" jstring
        jexpect "there" [||]                                      jhello <| jobj
        jexpect ""      [|jerr (JError.MemberNotFound "there")|]  jthere <| jobj
        jexpect ""      [|jerr JError.NotAnObject|]               jhello <| jarr
        jexpect ""      [|jerr JError.NotAnObject|]               jhello <| jws

      let run () =
        test_jisNull  ()
        test_jbool    ()
        test_jfloat   ()
        test_jstring  ()
        test_jvalue   ()
        test_jindex   ()
        test_jmember  ()

    let run () =
      Primitives.run ()
      Extractors.run ()

  let run () =
    Functionality.run ()
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
