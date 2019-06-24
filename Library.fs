module ParserApp
open ParserBuildingBlocks

[<EntryPoint>]
let main argv =
    printfn "Hello world"
    examplesForTestingParserBuildingBlocks
    System.Console.ReadKey() |> ignore
    0