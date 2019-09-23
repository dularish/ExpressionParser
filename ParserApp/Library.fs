module ParserApp
open ParserBuildingBlocks
open MathematicalExpressionParser
[<EntryPoint>]
let main argv =
    printfn "Hello world"
    BasicParsers.examplesForTestingParserBuildingBlocks
    RefractoredImpl.refractoredImplExamples()
    System.Console.ReadKey() |> ignore
    0