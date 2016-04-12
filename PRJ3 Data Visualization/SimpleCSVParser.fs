module CSVParser

open System
open ParserMonad

type CSVTOKEN =
  | DELIMITER
  | INFO of string
  | INVALID
  with
  member this.getValueUNSAFE =
    match this with
    | INFO(x) -> x
    | DELIMITER -> failwith "there is a delimiter where it shouldnt be"
    | INVALID -> "null"

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

let checkNewLine () =
  parse{
    let! x = getHead()
    if x = '\n' then
      return ()
    else
      return! fail_ "error in newline check"
  }
  
let checkDelimiter d =
  parse{
    let! x = getHead()
    if x = d then
      return ()
    else
      return! fail_ "error in delimiter check"
  }

let readChar d =
  parse{
    let! x = getHead()
    if x <> d && x <> '\n' then
      return x
    else
      return! fail_ "error in character check"
  }

let readString d =
  parse{
    let! str = repeatMultiParse (readChar d)
    return INFO(List.fold (fun s x -> s+(x.ToString())) "" str)
  }

//LEXING
let rec SimplerLexer d = //for reading 1 line
  parse{
    let! x = readString d
    do! checkDelimiter d
    let! xs = SimplerLexer d
    return x::xs
  }.||
  parse{
    do! checkDelimiter d
    let! xs = SimplerLexer d
    return CSVTOKEN.INVALID::xs
  }.||
  parse{
    do! checkNewLine()
    return []
  }

let rec SimpleLexer d : (char list -> Result<CSVTOKEN list list, char list>) = //for reading multiple lines
  let rec readLine() : (char list -> Result<CSVTOKEN list, char list>)=
    let cont() = 
      parse{
        do! checkDelimiter d
        let! xs = readLine()
        return xs
      }.||
      parse{
        do! checkNewLine()
        return []
      }.||
      parse{
        do! getEOF()
        return []
      }
    parse{
      let! value = readString d
      let! next = cont()
      return value::next
    }.||
    parse{
      do! checkDelimiter d
      let! xs = cont()
      return INVALID::xs
    }
  parse{
    let! currentline = readLine()
    let! nextline = SimpleLexer d
    return currentline::nextline
  }.||
  parse{
    do! getEOF()
    return []
  }

let Transform lines =
  List.map (fun line -> List.fold (fun s item ->  match item with
                                                  | DELIMITER -> s
                                                  | INVALID -> None::s
                                                  | INFO(x) -> Some(x)::s) [] line) lines



//following is the sample code for lexing some string


//below is the "parser" that actually works, above is a testment to my stupidity and a monumental waste of time, literally everything there is useless
let FoldParser (d:char) (chars:string) =
  //printfn "%A" chars
  let _, tokens =
    List.fold (fun (string, tokens) x ->  match x with
                                          | x when x = d -> if string = "" then
                                                              (string, tokens@[CSVTOKEN.INVALID])
                                                            else
                                                              ("", tokens@[INFO(string)])
                                          | x -> (string+(x.ToString()), tokens)
                                          | _ -> ("", tokens)) ("", []) (chars |> List.ofSeq)
  //printfn "%A" tokens
  tokens