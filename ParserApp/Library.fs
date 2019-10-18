module ParserApp
[<EntryPoint>]
let main argv =
    printfn "Hello world"
    FParsecExpressionEvaluator.refractoredImplExamples()
    System.Console.ReadKey() |> ignore
    0