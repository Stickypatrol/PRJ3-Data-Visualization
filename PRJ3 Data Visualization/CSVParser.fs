module CSVParser

//may not use this
(*
open System
open ParserMonad
open AuxTypes

type Token =
  | DELIMITER
  | NEWLINE
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

let readNewLine () =
  parse{
    let! x = getHead()
    if x = '\n' then
      return NEWLINE
    else
      return! fail_ "error in newline check"
  }

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
    if ((x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z') || x = ' ') && x <> d && not (List.exists ((=) x) symbols) then
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

//LEXING
let rec Lexer d symbols =
  parse{
    let! x =
      (readNewLine()) .||
      (readDelimiter d) .||
      (readSymbol symbols) .||
      (readInteger()) .||
      (readString d symbols)
    let! xs = Lexer d symbols
    return x::xs
  } .||
  parse{
    do! getEOF()
    return []
  }

//PARSING
(*
ID int - int
date int / int
incidentcode string
description string
district # string int
area(s) string / string / ... string
place string
neighbourhood int string
street string
day str
year int
month int
part of day int*:*int*-*int*:*int
biketype string
objecttype string
brand string
bikemodel | string | int string
colour string
*)

let checkSTRING() =
  parse{
    let! x = getHead()
    match x with
    | STRING(x) -> return x
    | _ -> return! fail_ ("expected string, got " + x.ToString())
  }

let checkINT() =
  parse{
    let! x = getHead()
    match x with
    | INT(x) -> return x
    | _ -> return! fail_ ("expected int, got " + x.ToString())
  }

let checkSLASH() =
  parse{
    let! x = getHead()
    match x with
    | SYMBOL(x) when x = '/' -> return x
    | _ -> return! fail_ ("expected slash, got " + x.ToString())
  }

let checkCOLON() =
  parse{
    let! x = getHead()
    match x with
    | SYMBOL(x) when x = ':' -> return x
    | _ -> return! fail_ ("expected colon, got " + x.ToString())
  }

let checkHYPHEN() =
  parse{
    let! x = getHead()
    match x with
    | SYMBOL(x) when x = '-' -> return x
    | _ -> return! fail_ ("expected hyphen, got " + x.ToString())
  }

let checkDOT() =
  parse{
    let! x = getHead()
    match x with
    | SYMBOL(x) when x = '.' -> return x
    | _ -> return! fail_ ("expected dot, got " + x.ToString())
  }

let testDELIMITER =
  fun s ->
    match s with
    | h::t when h = DELIMITER -> Success((), t)
    | _ -> Fail("expected delimiter instead got something else")

let checkID() =
  parse{
    let! x = checkINT()
    let! y = checkHYPHEN()
    let! z = checkINT()
    do! testDELIMITER
    return ID(x, z)
  }

let checkDATE() =
  parse{
    let! day = checkINT()
    let! _ = checkSLASH()
    let! month = checkINT()
    let! _ = checkSLASH()
    let! year = checkINT()
    do! testDELIMITER
    return DATE(day, month, year)
  }

let checkTIMERANGE() =
  parse{
    let! a = checkINT()
    let! _ = checkCOLON()
    let! b = checkINT()
    let! _ = checkHYPHEN()
    let! x = checkINT()
    let! _ = checkCOLON()
    let! y = checkINT()
    do! testDELIMITER
    return TIMERANGE(a, b, x, y)
  }

let checkTIME() =
  parse{
    let! a = checkINT()
    let! _ = checkCOLON()
    let! b = checkINT()
    do! testDELIMITER
    return TIME(a, b)
  }

let checkDISTRICT() =
  parse{
    let! _ = checkSTRING()
    let! x = checkINT()
    do! testDELIMITER
    return DISTRICT(x)
  }

let rec checkNextAREA() =
  parse{
    do! testDELIMITER
    let! x = checkSTRING()
    let! xs = checkNextAREA()
    return x::xs
  }.||
  parse{
    do! testDELIMITER
    return []
  }

let rec checkAREA() =
  parse{
    let! x = checkSTRING()
    let! xs = checkNextAREA()
    return x::xs
  }

let checkPLACE() =
  parse{
    let! x = checkINT()
    let! y = checkSTRING()
    do! testDELIMITER
    return PLACE(x, y)
  }

let checkSTREET() =
  parse{
    let! x = checkSTRING()
    do! testDELIMITER
    return STREET(x)
  }

let checkDAY() =
  parse{
    let! x = checkSTRING()
    do! testDELIMITER
    return DAY(x)
  }

let checkBIKETYPE() =
  parse{
    let! x = checkSTRING()
    do! testDELIMITER
    return BIKETYPE(x)
  }

let checkOBJECTTYPE() =
  parse{
    let! x = checkSTRING()
    do! testDELIMITER
    return OBJECTTYPE(x)
  }

let checkBRAND() =
  parse{
    let! x = checkSTRING()
    do! testDELIMITER
    return BRAND(x)
  }

let checkBIKEMODEL() =
  parse{
    let! x = checkSTRING()
    do! testDELIMITER
    return BIKEMODEL(x)
  }

let checkCOLOR() =
  parse{
    let! x = checkSTRING()
    do! testDELIMITER
    return COLOR(x)
  }

let checkINVALID() =
  parse{
    do! testDELIMITER
    return INVALID
  }

let Parser() : Parser<'a, Token list> =
  parse{
    let! id = (checkID()) .|| (checkINVALID())
    let! date = (checkDATE()) .|| (checkINVALID())
    let! 
  }*)