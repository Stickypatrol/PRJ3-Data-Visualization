module CSVParser

open System
open ParserMonad

type Token =
  | DELIMITER
  | INT of int
  | STRING of string
  | SYMBOL of char

let fail_ x =
  fun s ->
    Fail(x)

let getHead() =
  fun s ->
    match s with
    | h::t -> Success(h, t)
    | [] -> Fail "error in gethead"

let getEOF() =
  fun s ->
    match s with
    | h::t -> Fail "error in getEOF"
    | [] -> Success((), [])

let readNumeric() =
  parse{
    let! x = getHead()
    if x >= '0' && x <= '9' then
      return x
    else
      return! fail_ "error in Numeric check"
  }

let readAlphanum d symbols =
  parse{
    let! x = getHead()
    if ((x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z')) && x <> d && not (List.exists ((=) x) symbols) then
      return x
    else
      return! fail_ "error in Numeric check"
  }

let readSymbol symbols =
  parse{
    let! x = getHead()
    if List.exists ((=) x) symbols then
      return SYMBOL(x)
    else
      return! fail_ "error in symbol check"
  }

let readDelimiter d =
  parse{
    let! x = getHead()
    if x = d then
      return DELIMITER
    else
      return! fail_ "error in delimiter check"
  }

let readInteger() =
  parse{
    let! integer = repeatMultiParse (readNumeric())
    return INT(Int32.Parse(List.fold (fun s x -> s+(x.ToString())) "" (List.rev integer)))
  }

let readString d symbols =
  parse{
    let! str = repeatMultiParse (readAlphanum d symbols)
    return STRING(List.fold (fun s x -> s+(x.ToString())) "" str)
  }

let rec Tokenizer d symbols =
  parse{
    let! x =
      (readDelimiter d) .||
      (readSymbol symbols) .||
      (readInteger()) .||
      (readString d symbols)
    let! xs = Tokenizer d symbols
    return x::xs
  } .||
  parse{
    do! getEOF()
    return []
  }