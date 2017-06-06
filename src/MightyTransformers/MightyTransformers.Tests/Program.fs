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

module AnyTransformerTest =
  open Common
  open MightyTransformers.Any.AnyTransformer
  open MightyTransformers.Any.AnyTransformer.AnyTransform

  let awarn p e = AnyErrorItem.New true   p e
  let afail p e = AnyErrorItem.New false  p e

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

  let test_authorsTransform () =
    let toObj  v                      = v :> obj
    let toMap (vs : (string*obj) [])  = vs |> Map.ofArray :> obj

    let authors = [|
        toMap [|
          "name"    , toObj "Ludwig"
          "surname" , toObj "Wittgenstein"
          "works"   , toObj [|
              toMap [|
                "kind"    , toObj "book"
                "title"   , toObj "Tractatus Logico-Philosophicus"
              |]
              toMap [|
                "kind"    , toObj "book"
                "title"   , toObj "Philosophical Investigations"
              |]
              toMap [|
                "kind"    , toObj "manuscript"
                "title"   , toObj "Notes on Logic"
              |]
            |]
        |]
        toMap [|
          "name"    , toObj "Rene"
          "surname" , toObj "Descartes"
          "birth"   , toObj "1596"
          "works"   , toObj [|
              toMap [|
                "kind"    , toObj "book"
                "title"   , toObj "Discourse on Method and the Meditations"
              |]
              toMap [|
                "kind"    , toObj "book"
                "title"   , toObj "Meditations and Other Metaphysical Writings"
              |]
              toMap [|
                "kind"    , toObj "notes"
                "title"   , toObj "Some unfinished notes"
              |]
            |]
        |]
      |]

    let akind kind =
      amember "kind" ()
        (aasString >>= fun v -> if v = kind then areturn () else afailuref () "Expected kind '%s' but found '%s'" kind v)

    let astr k      = amember k "" aasString
    let atitle      = astr "title"
    let abook       = akind "book"        >>. atitle |>> Book
    let amanuscript = akind "manuscript"  >>. atitle |>> Manuscript
    let awork       = abook <|> amanuscript
    let aworks      = amember "works" [||] (amany awork) |> asuppress
    let aauthor     =
      (astr "name") <&> (astr "surname")
      >>= fun (name, surname) ->
        apure (Author.New name surname)
        <*> (amemberz "birth" aasInt32 |> atoOption)
        <*> aworks
        |> awithContext (sprintf "%s %s" name surname)
    let aauthors    = amany aauthor

    let adapterRepo =
      let adapterRepo = AnyAdapterRepository ()
      adapterRepo.AddLookup   AnyAdapter.mapLookup<obj>
      adapterRepo.AddIterator AnyAdapter.mapIterator<obj>
      adapterRepo.AddIndexer  AnyAdapter.arrayIndexer<obj>
      adapterRepo.AddIterator AnyAdapter.arrayIterator<obj>
      adapterRepo

    let a = arun aauthors authors adapterRepo
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
        Author.New
          "Rene"
          "Descartes"
          (Some 1596)
          [|
            Book "Discourse on Method and the Meditations"
            Book "Meditations and Other Metaphysical Writings"
          |]
      |],
      [|
        awarn "json.[1](Rene Descartes).works.[2].kind" <| AnyError.Message "Expected kind 'book' but found 'notes'"
        awarn "json.[1](Rene Descartes).works.[2].kind" <| AnyError.Message "Expected kind 'manuscript' but found 'notes'"
      |]
    expect e a

    ()

module JsonTransformerTest =
  open Common
  open MightyTransformers.Json.JsonTransformer
  open MightyTransformers.Json.JsonTransformer.JTransform
  open MightyTransformers.Json.JsonTransformer.JTransform.Infixes

  open MiniJson.JsonModule

  let jwarn p e = JErrorItem.New true   p e
  let jfail p e = JErrorItem.New false  p e

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
    let jworks      = jmember "works" [||] (jmany jwork) |> jsuppress
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
          Author.New
            "Rene"
            "Descartes"
            (Some 1596.)
            [|
              Book "Discourse on Method and the Meditations"
              Book "Meditations and Other Metaphysical Writings"
            |]
        |],
        [|
          jwarn "json.[1](Rene Descartes).works.[2].kind" <| JError.Message "Expected kind 'book' but found 'notes'"
          jwarn "json.[1](Rene Descartes).works.[2].kind" <| JError.Message "Expected kind 'manuscript' but found 'notes'"
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
    let jres0     = jfail "json(0)" <| JError.Message "Error"
    let jres1     = jfail "json(1)" <| JError.Message "Error"
    let jres2     = jfail "json(2)" <| JError.Message "Error"

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

      let test_jsuppress () =
        // TODO: Suppress interacts with many combinators
        //  Test combinators like jmany, jbind,
        //  alternative all combinators should have a suppression test in them
        let je = jwarn "json(1)" <| JError.Message "Error"
        jexpect 1  [||]                 <| jsuppress jok1
        jexpect 0  [|je|]               <| jsuppress jerr1

      let test_jtoOption () =
        jexpect (Some 1)  [||]          <| jtoOption jok1
        jexpect None      [||]          <| jtoOption jerr1

#if FSHARP_41
      let test_jtoResult () =
        jexpect (Good 1)        [||]    <| jtoResult jok1
        jexpect (Bad [|jres1|]) [||]    <| jtoResult jerr1
#endif

      let test_junpack () =
        let j t = junpack (Choice1Of2 >> jreturn) (Choice2Of2 >> jreturn) t
        jexpect (Choice1Of2 1)          [||]  <| j jok1
        jexpect (Choice2Of2 [|jres1|])  [||]  <| j jerr1

      let test_jfailure () =
        let jres s = jfail "json" <| JError.Message s
        jexpect 0 [|jres "Hello"|]      <| jfailure 0 "Hello"
        jexpect 0 [|jres "There"|]      <| jfailure 0 "There"

      let test_jfailuref () =
        let jres s = jfail "json" <| JError.Message s
        jexpect 0 [|jres "Hello there"|]<| jfailuref 0 "Hello %s" "there"

      let test_jwarning () =
        let jres s = jwarn "json" <| JError.Message s
        jexpect 0 [|jres "Hello"|]      <| jwarning 0 "Hello"
        jexpect 0 [|jres "There"|]      <| jwarning 0 "There"

      let test_jwarningf () =
        let jres s = jwarn "json" <| JError.Message s
        jexpect 0 [|jres "Hello there"|]<| jwarningf 0 "Hello %s" "there"

      let test_jwithContext () =
        let jres s = jfail "json(Hello)(1)" <| JError.Message s
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
      let jres e  = jfail "json" e

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
        let je  = JErrorItem.New false "json.[0]" JError.NotAString
        jexpect ""      [|jres (JError.IndexOutOfRange -1)|]  jm1<| jobj
        jexpect ""      [|jres (JError.IndexOutOfRange -1)|]  jm1<| jarr
        jexpect "there" [||]                                  j0 <| jobj
        jexpect ""      [|je|]                                j0 <| jarr
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

module JsonTransformerTest2 =
  open System
  open System.IO
  open System.Globalization

  open MiniJson.JsonModule
  open MiniJson.DynamicJsonModule
  open MiniJson.DynamicJsonModule.Infixes

  type TimeEntry = TimeEntry of DateTime*String*float

  let test_authorsTransform () =
    let json = File.ReadAllText @"C:\temp\json\sample_response.json"
    match parse true json with
    | ParseResult.Success json      ->
      let fromDate    = DateTime.MinValue
      let toDate      = DateTime.MaxValue
      let parseDate s =
        let s = (s : string).Substring (0, 19)
        match DateTime.TryParseExact (s, "s", CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true  , dt  -> dt |> Some
        | false , _   -> None

      let result =
        seq {
          for issue in json.Query?issues.Children do
            let key = issue?key.AsString
            yield! seq {
              for history in issue?changelog?histories.Children do
                let timeItems   = history?items.Children |> Array.filter (fun item -> item?field.AsString = "timespent")
                let created     = history?created.AsString
                match parseDate created with
                | None          -> ()
                | Some created  when fromDate > created || toDate < created -> ()
                | Some created  ->
                  yield! seq {
                    for timeItem in timeItems do
                      let from  = timeItem?from.AsFloat
                      let to_   = timeItem?``to``.AsFloat
                      yield TimeEntry (created, key, to_ - from)
                    }
            }
        } |> Seq.toArray
      printfn "Success: %A" result
    | ParseResult.Failure (msg, _)  ->
      printfn "Failure: %A" msg

module JsonTransformerTest3 =
  open System
  open System.IO
  open System.Globalization

  open MightyTransformers.Json.JsonTransformer
  open MightyTransformers.Json.JsonTransformer.JTransform
  open MightyTransformers.Json.JsonTransformer.JTransform.Infixes

  open MiniJson.JsonModule

  type TimeEntry =
    | TimeEntry of DateTime*String*float

    static member Zero = TimeEntry (DateTime.MinValue, "", 0.0)

  let test_authorsTransform () =
    let json = File.ReadAllText @"C:\temp\json\sample_response.json"

    let parseDate s =
        let s = (s : string).Substring (0, 19)
        match DateTime.TryParseExact (s, "s", CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true  , dt  -> dt |> Some
        | false , _   -> None

    let jdateTime =
      jasString |>> parseDate >>=
        function
          | Some dt -> jreturn dt
          | _ -> jfailure DateTime.MinValue "Invalid date"

    let zero = DateTime.MinValue

    let jfield ft =
      jmember "field" ()
        (jasString >>= fun v -> if v = ft then jreturn () else jfailuref () "Expected field '%s' but found '%s'" ft v)

    let jisTimespent = jfield "timespent"

    match parse true json with
    | ParseResult.Success json      ->
      let jtimeentry key created =
        jtransform {
          do!     jisTimespent
          let!    from = jmemberz "from"  jasFloat
          let!    to_  = jmemberz "to"    jasFloat
          return  TimeEntry (created, key, to_ - from)
        } |> jsuppress
      let jtimentries key created = jmember "items"     [||]  (jmany (jtimeentry key created))
      let jhistory    key         = jmember "created"   zero  jdateTime               >>= jtimentries key
      let jhistories  key         = jmember "histories" [||]  (jmany (jhistory key))  |>> Array.concat
      let jchangelog  key         = jmember "changelog" [||]  (jhistories key)
      let jissue                  = jmember "key"       ""    jstring                 >>= jchangelog
      let jissues                 = jmember "issues"    [||]  (jmany jissue)          |>> Array.concat

      let result                  = jrun jissues json

      printfn "Success: %A" result
    | ParseResult.Failure (msg, _)  ->
      printfn "Failure: %A" msg

module XmlTransformerTest =
  open Common
  open MightyTransformers.Xml.XmlTransformer
  open MightyTransformers.Xml.XmlTransformer.XElementQueries
  open MightyTransformers.Xml.XmlTransformer.XElementQueries.Infixes
  open MightyTransformers.Xml.XmlTransformer.XTransform
  open MightyTransformers.Xml.XmlTransformer.XTransform.Infixes
  open System.Xml

  let xwarn p e = XErrorItem.New true   p e
  let xfail p e = XErrorItem.New false  p e

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

  let test_authorsTransform () =
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
    let xworks        = (xelements xqtrue xwork) |> xsuppress
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
        Author.New
          "Rene"
          "Descartes"
          (Some 1596)
          [|
            Book "Discourse on Method and the Meditations"
            Book "Meditations and Other Metaphysical Writings"
          |]
      |],
      [|
        xwarn "./author'1(Rene Descartes)/notes'2" <| XError.CheckFailed "Expected element named 'book'"
        xwarn "./author'1(Rene Descartes)/notes'2" <| XError.CheckFailed "Expected element named 'manuscript'"
      |]
    let a = xrun xauthors [||] xdoc.DocumentElement

    expect e a

module JsonSamples =
  open Common
  open MightyTransformers.Json.JsonTransformer
  open MightyTransformers.Json.JsonTransformer.JTransform
  open MightyTransformers.Json.JsonTransformer.JTransform.Infixes

  open MiniJson.JsonModule

  let sample1 () =
    let t : JTransform<float []>   = jmany jfloat
    let r : float []*JErrorItem [] = jrunOnString true t [||] """[1,2,3,4]"""
    printfn "sample1: %A" r

  let sample2 () =
    let t : JTransform<float []>   = jmany jfloat
    let r : float []*JErrorItem [] = jrunOnString true t [||] """[1,"2",3]"""
    printfn "sample2: %A" r

  let sample3 () =
    let t : JTransform<float []>   = jmany jasFloat
    let r : float []*JErrorItem [] = jrunOnString true t [||] """[1,"2",3]"""
    printfn "sample3: %A" r

  type Customer =
    {
      FirstName   : string
      LastName    : string
      YearOfBirth : int
    }

    static member New firstName lastName yearOfBirth : Customer =
      {
        FirstName   = firstName
        LastName    = lastName
        YearOfBirth = yearOfBirth
      }

    static member Empty = Customer.New "" "" 0

  let sample4 () =
    let customer = """
{
    "name"    : "Rene"
  , "surname" : "Descartes"
  , "birth"   : 1596
}
"""
    let jcustomer =
      jtransform {
        let! name     = jmember   "name"    ""  jstring
        let! surname  = jmember   "surname" ""  jstring
        let! birth    = jmemberz  "birth"       jfloat  |>> int
        return Customer.New name surname birth
      }
    let r = jrunOnString true jcustomer Customer.Empty customer
    printfn "sample4: %A" r

  let sample5 () =
    let customers = """
[
    {
        "name"    : "Ludwig"
      , "surname" : "Wittgenstein"
    }
  , {
        "name"    : "Rene"
      , "surname" : "Descartes"
      , "birth"   : 1596
    }
]
"""
    let jcustomer =
      jtransform {
        let! name     = jmember   "name"    ""  jstring
        let! surname  = jmember   "surname" ""  jstring
        let! birth    = jmemberz  "birth"       jfloat  |>> int
        return Customer.New name surname birth
      }
    let jcustomers = jmany jcustomer
    let r = jrunOnString true jcustomers [||] customers
    printfn "sample5: %A" r

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

  let sample6 () =
    let authors = """
[
    {
        "name"    : "Ludwig"
      , "surname" : "Wittgenstein"
    }
  , {
        "name"    : "Rene"
      , "surname" : "Descartes"
      , "birth"   : 1596
    }
]
"""
    let jauthor =
      jtransform {
        let! name     = jmember   "name"    ""  jstring
        let! surname  = jmember   "surname" ""  jstring
        let! birth    = jmemberz  "birth"       jfloat  |>> int |> jtoOption
        return Author.New name surname birth [||]
      }
    let jauthors = jmany jauthor
    let r = jrunOnString true jauthors [||] authors
    printfn "sample6: %A" r

  let sample7 () =
    let authors = """
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
    let jexpectString e =
      jtransform {
        let! a = jstring
        if e = a then
          return ()
        else
          return! jfailuref () "Expected '%s' but found '%s'" e a
      }
    let jhasKind kind = jmember "kind" () (jexpectString kind)
    let jstr k = jmember k "" jasString
    let jbook =
      jtransform {
        do! jhasKind "book"
        let! jtitle = jstr "title"
        return Book jtitle
      }
    let jmanuscript =
      jtransform {
        do! jhasKind "manuscript"
        let! jtitle = jstr "title"
        return Manuscript jtitle
      }
    let jwork = jbook <|> jmanuscript
    let jworks = jmany jwork
    let jauthor =
      let inner name surname =
        jtransform {
          let! birth    = jmemberz  "birth"         jfloat  |>> int |> jtoOption
          let! works    = jmember   "works"   [||]  jworks
          return Author.New name surname birth works
        } |> jwithContext (sprintf "%s %s" name surname)
      jtransform {
        let! name     = jstr "name"
        let! surname  = jstr "surname"
        return! inner name surname
      }
    let jauthors = jmany jauthor
    let r = jrunOnString true jauthors [||] authors
    printfn "sample7: %A" r

  let run () =
    sample1 ()
    sample2 ()
    sample3 ()
    sample4 ()
    sample5 ()
    sample6 ()
    sample7 ()

open Common
open System

[<EntryPoint>]
let main argv =
  try
    Environment.CurrentDirectory <- AppDomain.CurrentDomain.BaseDirectory

    //runAllTests     ()
    //JsonSamples.run ()

//    JsonTransformerTest2.test_authorsTransform ()
//    JsonTransformerTest3.test_authorsTransform ()

    AnyTransformerTest.test_authorsTransform ()

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
