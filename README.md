# F# Monadic Transforms for Xml, Html and JSON

There are many good tools to parse JSON into a object tree that represents the JSON text but usually in order for the data to be useful we need to transform JSON tree into an internal representation.

JSON data is dynamic by nature meaning we need our code to handle missing attributes or subtrees gracefully. In addition, we need like to map & filter the JSON in order to transform it into our internal representation.

The transformation process is similar to text parsing and as parsing described effectively using monadic combinators it makes sense to think that JSON transformation can be described effectively using monadic combinators.

Let's look at a simple example:

```fsharp
let t : JTransform<float []>   = jmany jfloat
let r : float []*JErrorItem [] = jrunOnString true t [||] """[1,2,3,4]"""
printfn "sample1: %A" r
```

Prints:

> ([|1.0; 2.0; 3.0; 4.0|], [||])

`jmany jfloat` defines a transformer that will try to interpret the JSON document as an array of floats. The result of `jrunOnString` is a pair of `float []` and `JErrorItem []`.

If no errors or warnings are detected the error item is empty meaning the value is valid. The reason a value is always returned is that it's up to the calling code to decide if the errors are critical or the result is correct enough to use.

Let's say we pass a valid JSON string that don't match the transformer `[1, "2", 3]`, the result is then:

> sample2: ([|1.0; 3.0|], [|{IsSuppressed = false;
>                   Path = "json.[1]";
>                   Error = NotAFloat;}|])

The transformation was only partially successful here as the second element of the array is a string not a float. The transform indicates this by the path: `"json.[1]"` and error: `NotAFloat`. For each error detected during transformation process we get a `JErrorItem`.

We can make the transformation more relaxed by changing the example to:

```fsharp
let t : JTransform<float []>   = jmany jasFloat
let r : float []*JErrorItem [] = jrunOnString true t [||] """[1,"2",3]"""
printfn "sample3: %A" r
```

`jasFloat` tries to interpret the element as a float, the result is then:

> sample3: ([|1.0; 2.0; 3.0|], [||])

All good!

Let's look at a slightly more advanced example:

```json
{
    "name"    : "Rene"
  , "surname" : "Descartes"
  , "birth"   : 1596
}
```

Let's try to match this into an `Customer` type:

```fsharp
type Customer =
  {
    FirstName   : string
    LastName    : string
    YearOfBirth : float option
  }

  static member New firstName lastName yearOfBirth works : Customer =
    {
      FirstName   = firstName
      LastName    = lastName
      YearOfBirth = yearOfBirth
    }

  static member Empty = Customer.New "" "" 0
```

One way to transform the JSON document into a `Customer` type could be like this:

```fsharp
let jcustomer =
  jtransform {
    let! name     = jmember   "name"    ""  jstring
    let! surname  = jmember   "surname" ""  jstring
    let! birth    = jmemberz  "birth"       jfloat  |>> int
    return Customer.New name surname birth
  }
```

The way this works is that `jmember "name" "" jstring` tries to navigate to the member `name` in the current context, if the navigation succeeds `jstring` is applied to the data in order to interpret it as a string. The empty string is the value to return together with the error info in case the navigation fails. `jmemberz` is used for `float` as `float.Zero` exists and therefore we don't need to pass a value to return on error.

The result is:

> sample4: ({FirstName = "Rene";
>  LastName = "Descartes";
>  YearOfBirth = 1596;}, [||])

Let's change this into a transformer that tries to parse an array of customers:

```json
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
```

The transformer then looks like this:

```fsharp
let jcustomer =
  jtransform {
    let! name     = jmember   "name"    ""  jstring
    let! surname  = jmember   "surname" ""  jstring
    let! birth    = jmemberz  "birth"       jfloat  |>> int
    return Customer.New name surname birth
  }
let jcustomers = jmany jcustomer
```

`jmany` repeats the `jcustomer` for all elements in the JSON array. The result is:

> sample5: ([|{FirstName = "Rene";
>    LastName = "Descartes";
>    YearOfBirth = 1596;}|], [|{IsSuppressed = false;
>                               Path = "json.[0]";
>                               Error = MemberNotFound "birth";}|])

The transformer discovered that the first array element lacks a `birth` member which prevented it from adding the incomplete element to the array.

A simple way to fix this is to let the `birth` property be optional, at the same time we refine the type to be an `Author` with known works:

```fsharp
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
```

The transformer looks this (ignoring works for now):

```fsharp
let jauthor =
  jtransform {
    let! name     = jmember   "name"    ""  jstring
    let! surname  = jmember   "surname" ""  jstring
    let! birth    = jmemberz  "birth"       jfloat  |>> int |> jtoOption
    return Author.New name surname birth [||]
  }
let jauthors = jmany jauthor
```

`jtoOption` suppresses any faults when parsing `birth` and turn the value into an `int option`. The result is:

> sample6: ([|{FirstName = "Ludwig";
>   LastName = "Wittgenstein";
>   YearOfBirth = null;
>    Works = [||];}; {FirstName = "Rene";
>                     LastName = "Descartes";
>                     YearOfBirth = Some 1596;
>                     Works = [||];}|], [||])

There are other ways to suppress errors.


