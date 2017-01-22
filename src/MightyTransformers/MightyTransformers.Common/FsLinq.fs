namespace MightyTransformers.Common

// Provides F# idiomatic API around System.Linq.Enumerable
//  In general System.Linq.Enumerable performs better than Seq
[<RequireQualifiedAccess>]
module FsLinq =
  open System
  open System.Collections.Generic
  open System.Linq

  module Details =
    let inline adapt f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt f
    let inline equalsBy (by : 'T -> 'U when 'U :> IEquatable<'U>) =
      { new IEqualityComparer<'T> with
          override x.Equals (f, s) = (by f).Equals (by s)
          override x.GetHashCode f = (by f).GetHashCode ()
      }

    let inline compareBy (by : 'T -> 'U when 'U :> IComparable<'U>) =
      { new IComparer<'T> with
          override x.Compare (f, s) = (by f).CompareTo (by s)
      }

    let inline compareUsing (by : 'T -> 'T -> int) =
      let by = adapt by
      { new IComparer<'T> with
          override x.Compare (f, s) = by.Invoke (f, s)
      }

  open Details

  let inline range      (s : int)               (c : int)     : seq<int>= Enumerable.Range      (s, c)

  // ---

  let inline append     (f : seq<'T>)           (s : seq<'T>) : seq<'T> = Enumerable.Concat     (f, s)
  // average
  // averageBy
  // cache
  let inline cast<'T>   s                                     : seq<'T> = Enumerable.Cast<'T>   s
  // choose
  // chunkBySize
  let inline collect    (c : 'T -> seq<'U>)     (s : seq<'T>) : seq<'U> = Enumerable.SelectMany (s, Func<'T, seq<'U>> c)
  // compareWith
  let inline concat     (s : seq<seq<'T>>)                    : seq<'T> = collect id s
  let inline contains   (v : 'T)                (s : seq<'T>) : bool    = Enumerable.Contains   (s, v)
  // countBy
  // delay
  let inline distinct   (s : seq<'T>)                         : seq<'T> = Enumerable.Distinct   s
  let inline distinctBy (f : 'T -> 'U)          (s : seq<'T>) : seq<'T> = Enumerable.Distinct   (s, equalsBy f)
  [<GeneralizableValue>]
  let empty<'T>                                               : seq<'T> = upcast [||]
  let inline exactlyOne (s : seq<'T>)                         : 'T      = Enumerable.Single     s
  let inline except     (f : seq<'T>)           (s : seq<'T>) : seq<'T> = Enumerable.Except     (f, s)
  // exists
  // exists2
  let inline filter     (f : 'T -> bool)        (s : seq<'T>) : seq<'T> = Enumerable.Where      (s, Func<'T, bool> f)
  // find
  // findBack
  // findIndex
  // findIndexBack
  let inline fold       (f : 'S -> 'T -> 'S) z  (s : seq<'T>) : 'S      = Enumerable.Aggregate  (s, z, Func<'S, 'T, 'S> f)
  // fold2
  // foldBack
  // foldBack2
  let inline forall     (t : 'T -> bool)        (s : seq<'T>) : bool    = Enumerable.All        (s, Func<'T, bool> t)
  // forall2
  let inline groupBy    (f : 'T -> 'U)          (s : seq<'T>) : seq<_>  = Enumerable.GroupBy    (s, Func<'T, 'U> f)
  let inline head       (s : seq<'T>)                         : 'T      = Enumerable.First      s
  // indexed
  // init
  // initInfinite
  let inline isEmpty    (s : seq<'T>)                         : bool    = use e = s.GetEnumerator () in e.MoveNext () |> not
  // isEmpty
  // item
  // iter
  // iteri
  // iter2
  // iteri2
  let inline last       (s : seq<'T>)                         : 'T      = Enumerable.Last       s
  let inline length     (s : seq<'T>)                         : int     = Enumerable.Count      s
  let inline map        (m : 'T -> 'U)          (s : seq<'T>) : seq<'U> = Enumerable.Select     (s, Func<'T, 'U> m)
  // map2
  // map3
  // mapFold
  // mapFoldBack
  let inline mapi       (m : 'T -> int -> 'U)   (s : seq<'T>) : seq<'U> = Enumerable.Select     (s, Func<'T, int, 'U> m)
  // mapi2
  let inline max        (s : seq<'T>)                         : 'T      = Enumerable.Max        s
  // maxBy
  let inline min        (s : seq<'T>)                         : 'T      = Enumerable.Min        s
  // minBy
  let inline ofArray    (s : 'T [])                           : seq<'T> = upcast s
  let inline ofList     (s : 'T list)                         : seq<'T> = upcast s
  // pairwise
  // permute
  // pick
  // readonly
  let inline reduce     (f : 'T -> 'T -> 'T)    (s : seq<'T>) : 'T      = Enumerable.Aggregate  (s, Func<'T, 'T, 'T> f)
  // reduceBack
  // replicate
  let inline rev        (s : seq<'T>)                         : seq<'T> = Enumerable.Reverse    s
  // scan
  // scanBack
  let inline singleton  (v : 'T)                              : seq<'T> = upcast [|v|]
  let inline skip       (n : int)               (s : seq<'T>) : seq<'T> = Enumerable.Skip       (s, n)
  let inline skipWhile  (f : 'T -> bool)        (s : seq<'T>) : seq<'T> = Enumerable.SkipWhile  (s, Func<'T, bool> f)
  let inline sortBy     (f : 'T -> 'U)          (s : seq<'T>) : seq<'T> = Enumerable.OrderBy    (s, Func<'T, 'U> f) :> seq<'T>
  let inline sort       (s : seq<'T>)                         : seq<'T> = sortBy                id s
  let inline sortDescendingBy (f : 'T -> 'U)    (s : seq<'T>) : seq<'T> = Enumerable.OrderByDescending  (s, Func<'T, 'U> f) :> seq<'T>
  let inline sortDescending   (s : seq<'T>)                   : seq<'T> = sortDescendingBy      id s
  let inline sortWith   (f : 'T -> 'T -> int)   (s : seq<'T>) : seq<'T> = Enumerable.OrderBy    (s, Func<'T, 'T> id, compareUsing f) :> seq<'T>
  // sortWith
  // splitInto
  // sum
  // sumBy
  let inline tail       (s : seq<'T>)                         : seq<'T> = skip                  1 s
  let inline take       (n : int)               (s : seq<'T>) : seq<'T> = Enumerable.Take       (s, n)
  let inline takeWhile  (f : 'T -> bool)        (s : seq<'T>) : seq<'T> = Enumerable.TakeWhile  (s, Func<'T, bool> f)
  let inline toArray    (s : seq<'T>)                         : 'T []   = Enumerable.ToArray    s
  // toList
  let inline truncate   (n : int)               (s : seq<'T>) : seq<'T> = take                  n s
  // tryFind
  // tryFindBack
  // tryFindIndex
  // tryFindIndexBack
  // tryHead
  // tryItem
  // tryLast
  // tryTail
  // tryPick
  // unfold
  let inline where      (f : 'T -> bool)        (s : seq<'T>) : seq<'T> = filter                 f s
  // windowed
  // zip
  // zip3
