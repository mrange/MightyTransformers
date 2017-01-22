namespace MightyTransformers.Common

// In F#4.1 there's a Result<'T>
type Result<'TGood, 'TBad> =
  | Good  of 'TGood
  | Bad   of 'TBad

module Result =
  let inline toOption r =
    match r with
    | Good  good  -> good |> Some
    | Bad   _     -> None
