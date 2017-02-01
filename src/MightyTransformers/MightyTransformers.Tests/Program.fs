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

module Common =
  open FSharp.Core.Printf

  open System
  open System.Reflection
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

  let expect e a =
    if e <> a then
      errorf "%A <> %A" e a

  let runAllTestsIn (dt : Type) =
    let rec innerTypes (t : System.Type) =
      Seq.append (Seq.singleton t) (t.GetNestedTypes () |> (Seq.collect innerTypes))
    let filterMethod (mi : MethodInfo) =
      mi.IsStatic
      && mi.Name.StartsWith "test_"
      && (mi.GetParameters () |> Array.isEmpty)
    let tms =
      dt
      |> innerTypes
      |> Seq.collect  (fun t -> t.GetMethods ())
      |> Seq.filter   filterMethod
      |> Seq.toArray

    for tm in tms do
      infof "Running %s.%s" tm.DeclaringType.Name tm.Name
      try
        tm.Invoke (null, null) |> ignore
      with
      | e ->
        errorf "  %s.%s failed with: %s" tm.DeclaringType.Name tm.Name e.Message

  let inline runAllTests () =
    let mi  = MethodInfo.GetCurrentMethod ()
    let dt  = mi.DeclaringType
    runAllTestsIn dt

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

  let test_authorsTransform () =
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
      let e =
        [|
          Author.New
            "Ludwig"
            "Wittgenstein"
            None
            [|
              Book "Tractatus Logico-Philosophicus"
              Book "Philosophical Investigations"
              Manuscript "Notes on Logic"
            |]
        |],
        [|
          "root.[1](Rene Descartes).works.[2].kind", JError.Failure "Expected kind 'book' but found 'notes'"
          "root.[1](Rene Descartes).works.[2].kind", JError.Failure "Expected kind 'manuscript' but found 'notes'"
        |]
      let a = jrun jauthors json
      expect e a
    | ParseResult.Failure (msg, pos) ->
      errorf "Failed to parse json: %s" msg

    ()

  module Functionality =
    open MightyTransformers.Common

    open System.Reflection

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
        jexpect 1 [||]                  <| jreturn 1

      let test_jbind () =
        jexpect 2 [||]                  <| jbind jok1  jfok
        jexpect 0 [|jres1|]             <| jbind jok1  jferr
        jexpect 1 [|jres1|]             <| jbind jerr1 jfok
        jexpect 0 [|jres1; jres0|]      <| jbind jerr1 jferr

      let test_jarr () =
        jexpect 2 [||] <| jfok 1

      let test_jkleisli () =
        jexpect 3 [||]                  <| jkleisli jfok  jfok  1
        jexpect 0 [|jres2|]             <| jkleisli jfok  jferr 1
        jexpect 1 [|jres1|]             <| jkleisli jferr jfok  1
        jexpect 0 [|jres1; jres0|]      <| jkleisli jferr jferr 1

      let test_jpure () =
        jexpect 1 [||] <| jpure 1

      let test_japply () =
        let f = (+) 1
        jexpect 3 [||]                  <| japply (jpure f      ) jok2
        jexpect 1 [|jres2|]             <| japply (jpure f      ) jerr2
        jexpect 2 [|jres1|]             <| japply (jerr  id "1" ) jok2
        jexpect 0 [|jres1; jres2|]      <| japply (jerr  id "1" ) jerr2

      let test_jmap () =
        let f = (+) 1
        jexpect 2 [||]                  <| jmap f jok1
        jexpect 1 [|jres1|]             <| jmap f jerr1

      let test_jorElse () =
        jexpect 1 [||]                  <| jorElse jok1   jok2
        jexpect 1 [||]                  <| jorElse jok1   jerr2
        jexpect 2 [||]                  <| jorElse jerr1  jok2
        jexpect 0 [|jres1; jres2|]      <| jorElse jerr1  jerr2

      let test_jkeepLeft () =
        jexpect 1 [||]                  <| jkeepLeft jok1  jok2
        jexpect 1 [|jres2|]             <| jkeepLeft jok1  jerr2
        jexpect 0 [|jres1|]             <| jkeepLeft jerr1 jok2
        jexpect 0 [|jres1; jres2|]      <| jkeepLeft jerr1 jerr2

      let test_jkeepRight () =
        jexpect 2 [||]                  <| jkeepRight jok1  jok2
        jexpect 0 [|jres2|]             <| jkeepRight jok1  jerr2
        jexpect 2 [|jres1|]             <| jkeepRight jerr1 jok2
        jexpect 0 [|jres1; jres2|]      <| jkeepRight jerr1 jerr2

      let test_jpair () =
        jexpect (1,2) [||]              <| jpair jok1  jok2
        jexpect (1,0) [|jres2|]         <| jpair jok1  jerr2
        jexpect (0,2) [|jres1|]         <| jpair jerr1 jok2
        jexpect (0,0) [|jres1; jres2|]  <| jpair jerr1 jerr2

      let test_jtoOption () =
        jexpect (Some 1)  [||]          <| jtoOption jok1
        jexpect None      [||]          <| jtoOption jerr1

      let test_jtoResult () =
        jexpect (Good 1)        [||]    <| jtoResult jok1
        jexpect (Bad [|jres1|]) [||]    <| jtoResult jerr1

      let test_jfailure () =
        let jres s = "root", JError.Failure s
        jexpect 0 [|jres "Hello"|]      <| jfailure 0 "Hello"
        jexpect 0 [|jres "There"|]      <| jfailure 0 "There"

      let test_jfailuref () =
        let jres s = "root", JError.Failure s
        jexpect 0 [|jres "Hello there"|]<| jfailuref 0 "Hello %s" "there"

      let test_jwarning () =
        let jres s = "root", JError.Warning s
        jexpect 0 [|jres "Hello"|]      <| jwarning 0 "Hello"
        jexpect 0 [|jres "There"|]      <| jwarning 0 "There"

      let test_jwarningf () =
        let jres s = "root", JError.Warning s
        jexpect 0 [|jres "Hello there"|]<| jwarningf 0 "Hello %s" "there"

      let test_jwithContext () =
        let jres s = "root(Hello)(1)", JError.Failure s
        jexpect 1 [||]                  <| jwithContext "Hello" jok1
        jexpect 0 [|jres "Error"|]      <| jwithContext "Hello" jerr1

      let test_jdebug () =
        jexpect 1 [||]                  <| jdebug "Hello" jok1
        jexpect 0 [|jres1|]             <| jdebug "There" jerr1

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
      let jres e  = "root", e

      let test_jisNull () =
        jexpect true  [||]                    jisNull <| jnull
        jexpect false [||]                    jisNull <| jfalse

      let test_jbool () =
        jexpect false [||]                      jbool <| jfalse
        jexpect true  [||]                      jbool <| jtrue
        jexpect false [|jres JError.NotABool|]  jbool <| jnull

      let test_jfloat () =
        jexpect 0.  [||]                      jfloat  <| jfloat0
        jexpect 1.  [||]                      jfloat  <| jfloat1
        jexpect 0.  [|jres JError.NotAFloat|] jfloat  <| jnull

      let test_jstring () =
        jexpect ""      [||]                        jstring <| jws
        jexpect "Hello" [||]                        jstring <| jhello
        jexpect ""      [|jres JError.NotAString|]  jstring <| jnull

      let test_jvalue () =
        jexpect jws     [||]  jvalue  <| jws
        jexpect jhello  [||]  jvalue  <| jhello
        jexpect jnull   [||]  jvalue  <| jnull

      let test_jasBool () =
        jexpect false [||]    jasBool <| JsonNull
        jexpect false [||]    jasBool <| JsonBoolean false
        jexpect true  [||]    jasBool <| JsonBoolean true
        jexpect false [||]    jasBool <| JsonNumber 0.
        jexpect true  [||]    jasBool <| JsonNumber 2.5
        jexpect false [||]    jasBool <| JsonString ""
        jexpect true  [||]    jasBool <| JsonString "xx"
        jexpect false [||]    jasBool <| jarr
        jexpect false [||]    jasBool <| jobj

      let test_jasFloat () =
        let je = jres JError.CanNotConvertToFloat
        jexpect 0.   [||]    jasFloat <| JsonNull
        jexpect 0.   [||]    jasFloat <| JsonBoolean false
        jexpect 1.   [||]    jasFloat <| JsonBoolean true
        jexpect 0.   [||]    jasFloat <| JsonNumber 0.
        jexpect 2.5  [||]    jasFloat <| JsonNumber 2.5
        jexpect 0.   [|je|]  jasFloat <| JsonString ""
        jexpect 0.   [|je|]  jasFloat <| JsonString "xx"
        jexpect 3.25 [||]    jasFloat <| JsonString "325E-2"
        jexpect 0.   [|je|]  jasFloat <| jarr
        jexpect 0.   [|je|]  jasFloat <| jobj

      let test_jasString () =
        let je = jres JError.CanNotConvertToString
        jexpect ""        [||]    jasString <| JsonNull
        jexpect "false"   [||]    jasString <| JsonBoolean false
        jexpect "true"    [||]    jasString <| JsonBoolean true
        jexpect "0"       [||]    jasString <| JsonNumber 0.
        jexpect "2.5"     [||]    jasString <| JsonNumber 2.5
        jexpect ""        [||]    jasString <| JsonString ""
        jexpect "xx"      [||]    jasString <| JsonString "xx"
        jexpect ""        [|je|]  jasString <| jarr
        jexpect ""        [|je|]  jasString <| jobj

      let test_jindex () =
        let j i = jindex i "" jstring
        let j0  = j 0
        let j1  = j 1
        let j2  = j 2
        let jm1 = j -1
        jexpect ""      [|jres (JError.IndexOutOfRange -1)|]  jm1<| jobj
        jexpect ""      [|jres (JError.IndexOutOfRange -1)|]  jm1<| jarr
        jexpect "there" [||]                                  j0 <| jobj
        jexpect ""      [|"root.[0]", JError.NotAString|]     j0 <| jarr
        jexpect ""      [|jres (JError.IndexOutOfRange 1)|]   j1 <| jobj
        jexpect "hello" [||]                                  j1 <| jarr
        jexpect ""      [|jres (JError.IndexOutOfRange 2)|]   j2 <| jobj
        jexpect ""      [|jres (JError.IndexOutOfRange 2)|]   j2 <| jarr
        jexpect ""      [|jres JError.NotAnArrayOrObject|]    j0 <| jws

      let test_jmany () =
        let j = jmany jvalue
        jexpect [|JsonString "there"|]                    [||]                                j <| jobj
        jexpect [|JsonBoolean true; JsonString "hello"|]  [||]                                j <| jarr
        jexpect [||]                                      [|jres JError.NotAnArrayOrObject|]  j <| jws

      let test_jmember () =
        let jhello = jmember "hello" "" jstring
        let jthere = jmember "there" "" jstring
        jexpect "there" [||]                                      jhello <| jobj
        jexpect ""      [|jres (JError.MemberNotFound "there")|]  jthere <| jobj
        jexpect ""      [|jres JError.NotAnObject|]               jhello <| jarr
        jexpect ""      [|jres JError.NotAnObject|]               jhello <| jws

  let run () =
    runAllTests ()

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
    let e =
      [|
        Author.New
          "Ludwig"
          "Wittgenstein"
          None
          [|
            Book "Tractatus Logico-Philosophicus"
            Book "Philosophical Investigations"
            Manuscript "Notes on Logic"
          |]
      |],
      [|
        "./author@1(Rene Descartes)/notes@2", XError.CheckFailed "Expected element named 'book'"
        "./author@1(Rene Descartes)/notes@2", XError.CheckFailed "Expected element named 'manuscript'"
      |]
    let a = xrun xauthors [||] xdoc.DocumentElement

    expect e a

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
