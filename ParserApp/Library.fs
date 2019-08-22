module ParserApp
open ParserBuildingBlocks
open MathematicalExpressionParser
[<EntryPoint>]
let main argv =
    printfn "Hello world"
    examplesForTestingParserBuildingBlocks
    examplesForMathematicalExpressionParser
    RefractoredImpl.refractoredImplExamples()
    System.Console.ReadKey() |> ignore
    0