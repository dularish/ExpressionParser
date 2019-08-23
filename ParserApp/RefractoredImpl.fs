module RefractoredImpl
open MathematicalExpressionParser
open ParserBuildingBlocks
open System


let OrElseSelectiveUnwrapping parser1 parser2 =
    let innerFn input =
        if (String.length input) = 0 then
            Failure "No more inputs"
        else
            let result1 = run (parser1()) input
            match result1 with
            | Success (matched1, remaining) ->
                Success (matched1, remaining)
            | Failure err ->
                let result2 = run (parser2()) input
                match result2 with
                | Success (matched2,remaining2) ->
                    Success (matched2, remaining2)
                | Failure err ->
                    Failure err
    Parser innerFn        

let (<*|*>) = OrElseSelectiveUnwrapping

let parseSingleTerm =
    (opt (pChar '-')) .>>. (many1 parseDigit) .>>. (opt ((pChar '.') .>>. (many1 parseDigit) ))
    |>> (fun (wholeNums, decimalPart) ->
        let wholeNumsWithNegSignIfNeeded =
            match wholeNums with
            | Some _ , wholeNumsList ->
                '-' :: wholeNumsList
            | _ , wholeNumsList ->
                wholeNumsList
        match decimalPart with
        | Some (decimalPoint, decimalPartDigits) ->
            String(List.toArray(wholeNumsWithNegSignIfNeeded @ [decimalPoint] @ decimalPartDigits))
            |> double |> Expression.Constant
        | None ->
            String(List.toArray(wholeNumsWithNegSignIfNeeded)) |> double |> Expression.Constant)

let bracketedExpression expParser = fun() ->
    (between parseOpenBracket (parseSpaces >>. expParser() .>> parseSpaces) parseCloseBracket)

let parseTerm (expParser)=
    (fun () -> parseSingleTerm)
    <*|*> (bracketedExpression expParser)

type ShuntingYardStreamCandidateTypes =
    | MaybeOperator of Token option
    | Expr of Expression

let rec performShuntingYardLogicOnList expStack opStack streamList =
    match streamList with
    | [] ->
        match opStack with
        | stackTopOp :: restOps ->
            match expStack with
            | topMostExp :: secondTopExp :: restExps ->
                let newExprToPush = BinaryExpression (secondTopExp, stackTopOp, topMostExp)
                performShuntingYardLogicOnList (newExprToPush :: restExps) (restOps) []
            | _ ->
                Expression.Constant -1.
        | [] ->
            match expStack with
            | onlyStackExp :: [] ->
                onlyStackExp
            | _ ->
                //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                Expression.Constant -1.
    | topMostOfStream :: restStream ->
        match topMostOfStream with
        | MaybeOperator operator ->
            match operator with
            | None ->
                Expression.Constant -1.
            | Some ( BinaryOperator someOp) ->
                match opStack with
                | [] ->
                    performShuntingYardLogicOnList expStack (someOp :: opStack) restStream
                | opStackTop :: opStackRest ->
                    if ((getPriority(someOp) > getPriority(opStackTop)) || (((someOp = opStackTop) && ((getAssociativity someOp) = Right )))) then
                        performShuntingYardLogicOnList expStack (someOp :: opStack) restStream
                    else
                        match expStack with
                        | topMostExp :: secondTopExp :: restExps ->
                            let newExprToPush = BinaryExpression (secondTopExp, opStackTop, topMostExp)
                            performShuntingYardLogicOnList (newExprToPush :: restExps) (opStackRest) streamList
                        | _ ->
                            //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                            Expression.Constant -1.
            | _ ->
                Expression.Constant -1.
        | Expr someExp ->
            performShuntingYardLogicOnList (someExp :: expStack) opStack restStream

let convertContinuousTermsToSingleExpression (firstExp,(operatorExpPairList:(Token option * Expression)list)) =
    let secondArgumentConvertedToSingleList =
        match operatorExpPairList with
        | [] -> []
        | _ ->
            operatorExpPairList
            |> List.map (fun (x,y) -> [MaybeOperator x]@[Expr y])
            |> List.reduce (@)
    ((Expr firstExp) :: secondArgumentConvertedToSingleList)
    |> performShuntingYardLogicOnList [] []

let parseContinuousTerms expParser= fun() ->
    parseSpaces >>. ((parseTerm expParser) .>>. (many ((opt(parseSpaces >>. parseArithmeticOp .>> parseSpaces)) .>>. (parseTerm expParser)))) .>> parseSpaces
    |>> convertContinuousTermsToSingleExpression

let rec parseExpression = fun() ->
     (parseContinuousTerms parseExpression) <*|*> (bracketedExpression parseExpression)

let getParsedOutput inputString =
    run (parseExpression()) inputString

let parseAndEvaluateExpressionExpressively (expressionString) =
   let parsedExpression = getParsedOutput expressionString
   match parsedExpression with
   | Success (exp, remainingString) ->
        match remainingString.Trim().Length with
        | 0 ->
            ((EvaluateExpression (Some exp)), remainingString, Seq.ofList [])
        | _ ->
            (EvaluationFailure (ParsingError (MathExpressionParsingFailureType.IncompleteParsing (sprintf "Could not parse the remaining string : %A" remainingString))) , "", Seq.ofList [])
   | Failure failure ->
        (EvaluationFailure (ParsingError (MathExpressionParsingFailureType.UnhandledInput "")) , "", Seq.ofList [])
           

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

    let errorCases = 
        Map.empty
            .Add("(6 + c)", ExpressionEvaluationError.ParsingError (MathExpressionParsingFailureType.UnrecognizedInput "c should not be recognized"))
            .Add("  6 + c", ExpressionEvaluationError.ParsingError (MathExpressionParsingFailureType.UnrecognizedInput "c should not be recognized"))
            .Add("  7 & 2", ExpressionEvaluationError.ParsingError (MathExpressionParsingFailureType.UnrecognizedInput "& should not be recognized"))
            .Add("  %", ExpressionEvaluationError.UnexpectedToken "")
            .Add(" 5 + + 6", ExpressionEvaluationError.InvalidOperatorUse "")
            .Add("5/0", ExpressionEvaluationError.DivideByZeroAttempted "")
            .Add(" 2 - 1 + 14/0 + 7", ExpressionEvaluationError.DivideByZeroAttempted "")
            .Add("(5*7/5) + (23) - 5 * (98-4)/(6*7-42)", ExpressionEvaluationError.DivideByZeroAttempted "")
            .Add("2 + (5 * 2", ExpressionEvaluationError.UnBalancedParanthesis "")
            .Add("(((((4))))", ExpressionEvaluationError.UnBalancedParanthesis "")
            .Add("((2)) * ((3", ExpressionEvaluationError.UnBalancedParanthesis "")
            .Add("((9)) * ((1)", ExpressionEvaluationError.UnBalancedParanthesis "")

    let developerDeliberatedNotHandledCases =
        [
        "2 + 2(4)"; //Not supported by existing Simpla expression parser
        "(1 + 2)(1 + 4)"; //Not supported by existing Simpla expression parser
        "asin 0"; //Existing Simpla expression parser returns -Infinity
        "2*-2"//Existing simpla expression parser supports this//If this has to be supported, then the expression "2*--2" would also be supported
        ]

    printfn "\n\n\nRefractoredImpl\n\n"
    let printParsedOutput inputString =
        printfn "Original Expression : %A" inputString
        printfn "%A" (getParsedOutput inputString)

    let mutable countOfFailed = 0
    let mutable countOfSuccess = 0

    let printAllCases (key:string) (expectedValue: double) =
        let evaluatedResult = parseAndEvaluateExpressionExpressively key
        match evaluatedResult with
        | EvaluationFailure someString, _, variablesRef ->
            printfn "\nFailure to evaluate %s" key
            printfn "Evaluation Failure message : %A" someString
            countOfFailed <- countOfFailed + 1
        | EvaluationSuccess evaluatedValue , remaining , variablesRef->
            if remaining.Trim().Length > 0 then
                printfn "The following expression evaluated successfully but some text remaining"
                printfn "Expression : %A" key
                printfn "Remaining String : %A" remaining
                printfn "Variables Referenced : %A" variablesRef
            else
                printfn "Expression : %A" key
                printfn "Expected value : %A" expectedValue
                printfn "Obtained value : %A" evaluatedValue
                printfn "Variables Referenced : %A" variablesRef
                printfn "Result : %A" (if expectedValue = evaluatedValue then "Success" else "Failure")
                if expectedValue = evaluatedValue then
                    countOfSuccess <- countOfSuccess + 1
                else
                    countOfFailed <- countOfFailed + 1

    successTestCases |> Map.toSeq |> Seq.map fst |> Seq.iter printParsedOutput
    successTestCases |> Map.iter printAllCases
    printfn "\n\nNumber of failed cases : %A " countOfFailed
    printfn "Number of success cases : %A " countOfSuccess
    errorCases |> Map.toSeq |> Seq.map fst |> Seq.iter printParsedOutput
    errorCases |> Map.toSeq |> Seq.map fst |> Seq.iter (fun s -> printfn "ExpressionInput: %A\nEvaluatedOutput: %A" (s) (parseAndEvaluateExpressionExpressively s))
    printfn "Developer didn't handle it deliberately cases :"
    developerDeliberatedNotHandledCases |> List.iter (fun s -> printfn "ExpressionInput: %A\nEvaluatedOutput: %A" (s) (parseAndEvaluateExpressionExpressively s))