module CSVParser

open System
open ParserMonad

type Token =
  | Delimiter of char
  | Int of int
  | String of string
  | Symbol of char

let fail_ x =
  fun s ->
    Fail(x)

let getHead =
  fun s ->
    match s with
    | h::t -> Success(h, t)
    | [] -> Fail "error in gethead"

let getEOF =
  fun s ->
    match s with
    | h::t -> Fail "error in getEOF"
    | [] -> Success((), [])

let readNumeric =
  parse{
    let! x = getHead
    if x <= '9' && x >= '0' then
      return x
    else
      return! fail_ "error in trying Numeric check"
  }

let readAlphanum =
  parse{
    let! x = getHead
    if (x <= 'a' && x >= 'z') || (x <= 'A' && x >= 'Z') then
      return x
    else
      return! fail_ "error in trying Numeric check"
  }

let 