﻿module SampleStuff

open System
open CoroutineMonad
open CSVParser
//let sampledataGenerator x = [for y in 0.0 .. 0.05 .. 15.0 -> 10.0*(sin y - 20.0]

//console settings
Console.WindowWidth <- 144
Console.WindowHeight <- 46

let generateSin x offset = int (10.0*sin (((float x) + offset)/10.0)) + 26

let generateCos x offset = int (10.0*cos (((float x) + offset)/10.0)) + 26

let generateTan x offset = int (10.0*tan (((float x) + offset)/10.0)) + 26


let rec DrawGraph graph offset =
  for x in 1..143 do
    let point = graph x offset
    if point < 46 && point > 1 then
      Console.SetCursorPosition(x, point)
      Console.Write('X')

let rec drawloop yadd =
    let mutable buffer = ""
    for y in 1..46 do
      for x in 1..144 do
        let sine = int (25.0*sin (((float x) + yadd)/10.0)) - 26
        if x = 1 || y = 1 || y = 46 || x = 144 then
          buffer <- buffer + "="
        else // extra for COS and TAN|| int (10.0*cos (((float x) + yadd)/10.0)) - 10 = (y-52) || int (10.0*tan (((float x) + yadd)/10.0)) - 20 = (y-52)
          if sine = (y-46)  then //we take the SIN of the x axis and enlarge it and give it an offset, then we check that with equality with y, if true then we draw "X" else we draw " "
            buffer <- buffer + "X"
          else
            buffer <- buffer + " "
    Console.Clear()
    printfn "%s" buffer

let plotA() =
  cor{
    let! s = getCorState//get the state
    do drawloop s//draw a new graph with the state
    return ()//increment the state so we get a different graph from the last one
  }

let plotB() =
  cor{
    let! s = getCorState
    do Console.Clear()
    do DrawGraph generateSin s
    do DrawGraph generateCos s
    do DrawGraph generateTan s
    return ()
  }

let IncrState() =
  cor{
    let! s = getCorState
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