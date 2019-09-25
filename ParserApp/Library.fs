module ParserApp
open MathematicalExpressionParser
[<EntryPoint>]
let main argv =
    printfn "Hello world"
    RefractoredImpl.refractoredImplExamples()
    System.Console.ReadKey() |> ignore
    0