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

let [<Literal>]connectionStr = "server=145.24.200.232;user=mustafa;database=project;password=root"
let insert = "INSERT INTO `district`(`id`, `safety_index`, `name`, `location`) VALUES (null, 'bob', 'bob', 'bob')"
let select = "SELECT * FROM `district`"
let connection = new MySql.Data.MySqlClient.MySqlConnection(connectionStr)
do connection.Open()

let SendDistrictQuery index name loc =
  let newinsert = "INSERT INTO `district`(`id`, `safety_index`, `name`, `location`) VALUES (null, '"+index.ToString()+"', '"+name.ToString()+"', '"+loc.ToString()+"')"
  let command = new MySql.Data.MySqlClient.MySqlCommand(newinsert, connection)
  command.ExecuteNonQuery() |> ignore
  ()

let SendTheftQuery date time street code biketype ismotorized bikemodel district =
  let newinsert = "INSERT INTO `thefts`(`id`, `date`, `time`, `street`, `code`, `type`, `ismotorized`, `bikemodel`, `district_id`) VALUES (null, '"+date.ToString()+"', '"+time.ToString()+"', '"+street.ToString()+"', '"+code.ToString()+"', '"+biketype.ToString()+"', '"+ismotorized.ToString()+"', '"+bikemodel.ToString()+"', '"+district.ToString()+"')"
  let command = new MySql.Data.MySqlClient.MySqlCommand(newinsert, connection)
  command.ExecuteNonQuery() |> ignore
  ()

//let command = new MySql.Data.MySqlClient.MySqlCommand(insert,connection)
//command.ExecuteNonQuery() |> ignore
//let reader = command.ExecuteReader()
(*let data = reader |> Seq.unfold (fun (reader) ->
    if reader.Read() then
        Some (List.init reader.FieldCount (fun i -> reader.GetValue(i)), reader)
    else
        None) |> Seq.toList
printfn "%A" data*)

let delimiter = ','

let parseFunc data =
  fun s ->
    match FoldParser delimiter data with
    | result -> Done(result,s)

let ReadFunction inputfile table i (*i is a counter for number of queries*) =
  let StartReading =
    fun s ->
        let sr = new StreamReader (@"./" + inputfile)
        Done(sr, s)
  let getLine (sr:StreamReader) =
    fun s ->
      Done(sr.ReadLine(), s)
  let rec RecursiveReader reader i =
    cor{
      if i > 0 then
        let! newline = getLine reader
        let! parsedLine = parseFunc newline //list with 1 entry, which is a list of CSV tokens
        //insert the insert query here as a coroutine, then just repeat it until it returns yield
        //do printfn "%A" i
        //printfn "%A" parsedLine
        //printfn "%A" parsedLine
        let _, date, code, theftkind, _, district, neighbourhood, city, area, street, biketype, bikemodel, time =
            parsedLine.[0], parsedLine.[1], parsedLine.[2], parsedLine.[3], parsedLine.[4], parsedLine.[5],
            parsedLine.[6], parsedLine.[7], parsedLine.[8], parsedLine.[9], parsedLine.[22], parsedLine.[23], parsedLine.[12]
        let district' = Int32.Parse(List.fold (fun s x -> if x <= '9' && x >= '0' then s+(x.ToString()) else s) "" (district.getValueUNSAFE |> List.ofSeq))
        //printfn "date %A code %A theft %A district %A neighbourhood %A city %A area %A street %A biketype %A color %A time %A" date code theftkind district neighbourhood city area street biketype bikemodel time
        //let district' = List.fold (fun s x -> if x <= '9') 0 district.getValueUNSAFE |> List.ofSeq
        SendTheftQuery date.getValueUNSAFE time.getValueUNSAFE street.getValueUNSAFE code.getValueUNSAFE biketype.getValueUNSAFE
                  theftkind.getValueUNSAFE bikemodel.getValueUNSAFE district'
        printfn "sent query! %i queries to go!" i
        do! yield_ //we can add a pause here for whichever reason
        if reader.EndOfStream then
          return ()
        else
          return! RecursiveReader reader (i-1)
      else
        return ()
    }
  let FileCheck = fun s -> Done(File.Exists(@"./" + inputfile), s)
  cor{
    let! fileExist = FileCheck
    if fileExist then
        let! reader = StartReading
        let! _ = getLine reader
        do! RecursiveReader reader i
    else
        return ()
  }

let rec costep c s =
  match c s with
  | Done(x, s') -> ()
  | Yield(c', s') -> costep c' s'

//do costep (ReadFunction() 25) []
[<EntryPoint>]
let rec main argsv =
    //let argsv = [|"fietsdiefstaldata.csv"|]
    printfn "%A" argsv
    if argsv.Length > 0 then
        try
            for x in 0..argsv.Length - 1 do
                costep (ReadFunction argsv.[x] "thefts" 9900) ()
            0
        with
            | x ->  printfn "error happened: %A" (x.ToString())
                    0

    else
        //make a loop here that waits for requests
        0