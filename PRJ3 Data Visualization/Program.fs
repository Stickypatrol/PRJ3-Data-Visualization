open System
open Coroutine

//let sampledataGenerator x = [for y in 0.0 .. 0.05 .. 15.0 -> 10.0*(sin y - 20.0]

//console settings
Console.WindowWidth <- 150
Console.WindowHeight <- 54

let rec DrawGraph yadd =
  Console.Clear()
  for x in 1..149 do
    let sine = int (25.0*sin (((float x) + yadd)/10.0)) + 26
    if sine < 52 || sine > 1 then
      Console.SetCursorPosition(x, sine)
      Console.Write('X')

let rec drawloop yadd =
    let mutable buffer = ""
    for y in 1..52 do
      for x in 1..150 do
        let sine = int (25.0*sin (((float x) + yadd)/10.0)) - 26
        if x = 1 || y = 1 || y = 52 || x = 150 then
          buffer <- buffer + "="
        else // extra for COS and TAN|| int (10.0*cos (((float x) + yadd)/10.0)) - 10 = (y-52) || int (10.0*tan (((float x) + yadd)/10.0)) - 20 = (y-52)
          if sine = (y-52)  then //we take the SIN of the x axis and enlarge it and give it an offset, then we check that with equality with y, if true then we draw "X" else we draw " "
            buffer <- buffer + "X"
          else
            buffer <- buffer + " "
    Console.Clear()
    printfn "%s" buffer

let plotA() =
  cor{
    let! s = getState//get the state
    do drawloop s//draw a new graph with the state
    return ()//increment the state so we get a different graph from the last one
  }

let plotB() =
  cor{
    let! s = getState
    do DrawGraph s
    return ()
  }

let IncrState() =
  cor{
    let! s = getState
    do! setState (s - 2.0)
    return ()
  }
  
let A() =
  cor{
    do! wait 0.05
    do! plotB()
    do! IncrState()
  } |> repeat_

let rec costep c s =
  match c s with
  | Done(x, s') -> costep (cor{return x}) s'
  | Yield(c', s') -> costep c' s'

let sampleprogram() beginstate =
  costep (A()) beginstate

do sampleprogram() 0.0

//do drawloop 0.3