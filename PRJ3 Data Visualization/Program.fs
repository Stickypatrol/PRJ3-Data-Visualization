open System
open CoroutineMonad
open CSVParser
open AuxTypes
open ParserMonad
open System.IO
open System.Data
open System.Data.Linq
open Microsoft.FSharp.Data.TypeProviders
open Microsoft.FSharp.Linq
//let sampledataGenerator x = [for y in 0.0 .. 0.05 .. 15.0 -> 10.0*(sin y - 20.0]

let [<Literal>]connectionStr = "server=145.24.200.232;user=mustafa;database=test;password=root"
let query = "INSERT INTO `data`(`id`, `data`) VALUES (null, 'test')"
let connection = new MySql.Data.MySqlClient.MySqlConnection(connectionStr)
do connection.Open()

let command = new MySql.Data.MySqlClient.MySqlCommand(query,connection)
command.ExecuteNonQuery() |> ignore

let reader = command.ExecuteReader()
let data = reader |> Seq.unfold (fun (reader) ->
    if reader.Read() then
        Some (List.init reader.FieldCount (fun i -> reader.GetValue(i)), reader)
    else
        None) |> Seq.toList
printfn "%A" data
connection.Close()
connection.Dispose()

let delimiter = ','

let parseFunc data =
  fun s ->
    match SimplerLexer delimiter data with
    | Success(result, s') -> Done(result,s)
    | Fail(_) -> failwith "lexing failed"

let ReadFunction () i (*i is a counter for number of queries*)=
  let StartReading =
    fun s ->
      let sr = new StreamReader (@"C:\samplepics\fietsdiefstaldata.csv")
      Done(sr, s)
  let getLine (sr:StreamReader) =
    fun s ->
      Done(sr.ReadLine(), s)
  let rec RecursiveReader reader i =
    cor{
      if i > 0 then
        let! newline = getLine reader
        let! parsedLine = parseFunc (newline |> List.ofSeq) //list with 1 entry, which is a list of CSV tokens
        //insert the insert query here as a coroutine, then just repeat it until it returns yield
        do printfn "%A" parsedLine
        do! yield_ //we can add a pause here for whichever reason
        if reader.EndOfStream then
          return ()
        else
          return! RecursiveReader reader (i-1)
      else
        return ()
    }
  cor{
    let! reader = StartReading
    do! RecursiveReader reader i
    do! yield_
  }

let rec costep c s =
  match c s with
  | Done(x, s') -> costep (cor{return x}) s'
  | Yield(c', s') -> costep c' s'

//do costep (ReadFunction() 25) []
