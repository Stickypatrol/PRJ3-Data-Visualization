open System
open CoroutineMonad
open CSVParser

//let sampledataGenerator x = [for y in 0.0 .. 0.05 .. 15.0 -> 10.0*(sin y - 20.0]

//console settings
Console.WindowWidth <- 150
Console.WindowHeight <- 54

let generateSin x offset = int (20.0*sin (((float x) + offset)/10.0)) + 26

let generateCos x offset = int (20.0*cos (((float x) + offset)/10.0)) + 26

let generateTan x offset = int (10.0*tan (((float x) + offset)/10.0)) + 26


let rec DrawGraph graph offset =
  for x in 1..149 do
    let point = graph x offset
    if point < 52 && point > 1 then
      Console.SetCursorPosition(x, point)
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
    do Console.Clear()
    do DrawGraph generateSin s
    do DrawGraph generateCos s
    do DrawGraph generateTan s
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
    do! wait 0.1
    do! plotB()
    do! IncrState()
  } |> repeat_

let rec costep c s =
  match c s with
  | Done(x, s') -> costep (cor{return x}) s'
  | Yield(c', s') -> costep c' s'

let sampleprogram() beginstate =
  costep (A()) beginstate

//do sampleprogram() 0.0

let specialsymbols = ['/';':';'-';'.']

let delimiter = ','

let input = List.ofSeq <| "awdawdw,1231231,1/1/2009,10:00"

printfn "%A" (Lexer delimiter specialsymbols input)