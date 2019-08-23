module RefractoredImpl
open MathematicalExpressionParser
open ParserBuildingBlocks
open System

let parseSingleTerm =
    (many1 parseDigit) .>>. (opt ((pChar '.') .>>. (many1 parseDigit) ))
    |>> (fun (wholeNums, decimalPart) ->
        match decimalPart with
        | Some (decimalPoint, decimalPartDigits) ->
            String(List.toArray(wholeNums @ [decimalPoint] @ decimalPartDigits))
            |> double |> Expression.Constant
        | None ->
            String(List.toArray(wholeNums)) |> double |> Expression.Constant)

let parseTerm =
    parseSingleTerm// <|> (between parseOpenBracket (parseSpaces >>. parseSingleTerm .>> parseSpaces) parseCloseBracket)
    //<|> expParser

let convertContExpressionsToSingleExpression (firstExp,operatorExpPairList) =
    //Yet to write the logic
    Expression.BinaryExpression (Expression.Constant 1., Plus, Expression.Constant 1.)

let parseContinuousTerms =
    parseSpaces >>. (parseTerm .>>. (many (opt(parseSpaces >>. parseArithmeticOp .>> parseSpaces) .>>. parseTerm))) .>> parseSpaces
    |>> convertContExpressionsToSingleExpression

let parseExpression =
    parseContinuousTerms <|> (between parseOpenBracket (parseSpaces >>. parseContinuousTerms .>> parseSpaces) parseCloseBracket)

let getParsedOutput inputString =
    run parseExpression inputString

let refractoredImplExamples = fun() ->
    let successTestCases = 
        Map.empty
            //Region begin - TestcasesCreated by me
            .Add("23", 23. |> double)
            .Add("23 + 42",65.)
            .Add("(23 + 42)",65.)
            .Add("(23 + (22 + 43))",88.)
            .Add("(23 + 22) + 53",98.)
            .Add("((23 + 22) + 53)",98.)
            .Add("(23 + 24) + (25 + 26)",98.)
            .Add("21 + 22 + 23 + 24 + 25 + 26",141.)
            .Add("1 + 2 * 3 + 5 + 6 * 8",60.)
            .Add("1 - 2 * 3",-5.)
            .Add("1*(-3)",-3.)
            .Add("(1 + 2 + 3 * 3 * (1 + 2))", 30.)
            .Add("2 ^ 2 ^ 3", 256.)
            .Add("(2 ^ 2) ^ 3", 64.)
            .Add("4 % 5 * 5", 20.)
            //Region end - TestcasesCreated by me
            //Region begin - TestCases from https://lukaszwrobel.pl/blog/math-parser-part-4-tests/
            .Add("2 + 3", 5. |> double)
            .Add("2 * 3",6.)
            .Add("89", 89.0)
            .Add("   12        -  8   ", 4.)
            .Add("142        -9   ", 133.)
            .Add("72+  15", 87.)
            .Add(" 12*  4", 48.)
            .Add(" 50/10", 5.)
            .Add("2.5", 2.5)
            .Add("4*2.5 + 8.5+1.5 / 3.0", 19.)
            .Add("5.0005 + 0.0095", 5.01)
            .Add("67+2", 69.)
            .Add(" 2-7", -5.)
            .Add("5*7 ", 35.)
            .Add("8/4", 2.)
            .Add("2 -4 +6 -1 -1- 0 +8", 10.)
            .Add("1 -1   + 2   - 2   +  4 - 4 +    6", 6.)
            .Add(" 2*3 - 4*5 + 6/3 ", -12.)
            .Add("2*3*4/8 -   5/2*4 +  6 + 0/3   ", -1.)
            .Add("10/4", 2.5)
            .Add("5/3", 1.66)
            .Add("3 + 8/5 -1 -2*5", -6.4)
            .Add(" -5 + 2", -3.)
            .Add("(2)", 2.)
            .Add("(5 + 2*3 - 1 + 7 * 8)", 66.)
            .Add("(67 + 2 * 3 - 67 + 2/1 - 7)", 1.)
            .Add("(2) + (17*2-30) * (5)+2 - (8/2)*4", 8.)
            .Add("(((((5)))))", 5.)
            .Add("(( ((2)) + 4))*((5))", 30.)
    printfn "\n\n\nRefractoredImpl\n\n"
    let printParsedOutput inputString =
        printfn "Original Expression : %A" inputString
        printfn "%A" (getParsedOutput inputString)

    successTestCases |> Map.toSeq |> Seq.map fst |> Seq.iter printParsedOutput