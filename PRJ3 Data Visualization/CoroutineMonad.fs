module CoroutineMonad

open System

type Cor<'a, 's> = 's -> CorStep<'a, 's> //takes an 's (whatever it is) and returns either a DONE with a result and a new state (Done of 'a, 's) OR yield of new coroutine and new state
and CorStep<'a, 's> =
  | Done of 'a* 's
  | Yield of Cor<'a, 's>*'s

let ret x = fun s -> Done(x, s)

let rec bind p k =
  fun s ->
    match p s with
    | Done(x, s') -> k x s'
    | Yield(p', s') -> Yield((bind p' k), s')

type CoroutineBuilder() =
  member this.Return(x) = ret x
  member this.ReturnFrom(c) = c
  member this.Bind(p,k) = bind p k
  member this.Zero() = ret ()
let cor = CoroutineBuilder()

let yield_ = fun s -> Yield((fun s -> Done((), s)), s)

let getCorState =
  fun s ->
    Done(s, s)

let setState x =
  fun s ->
    Done((), x)

let rec repeat_ c =
  cor{
    do! c
    do! yield_
    return! repeat_ c
  }


let wait interval =
  let currentTime = fun s -> Done(DateTime.Now, s)
  cor{
    let! t0 = currentTime
    let rec wait() =
      cor{
        let! tc = currentTime
        let dt = (tc - t0).TotalSeconds
        if dt > interval then
          do! yield_
          return ()
        else
          do! yield_
          return! wait()
      }
    do! wait()
  }
