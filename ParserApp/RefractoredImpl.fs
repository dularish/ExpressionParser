module RefractoredImpl
open MathematicalExpressionParser
open FParsec
open BasicParsersForMathExpParsing
open HelperAlgosForMathExpParsing
open System

let parseVariableTerm=
    (parseVariableFromUserState
    >>= (fun (varKey, varValue, variablesDict, variablesRef, currentVarName) ->
                runParserOnString ((initializeRefVars (currentVarName:: variablesRef)) >>. (setVariableNameToUserState varKey) >>. (initializeVariablesDict variablesDict) >>. (globalExpParser)) UserState.Default varKey varValue
                |> (fun x ->
                        match x with
                        | Success ((ExpressionOutput (expressionParsed)), _, _) ->
                            preturn (ExpressionOutput(expressionParsed)) .>> (softPushRefVarForThisLayer varKey)
                        | Failure (label, err, pos) ->
                            fail (sprintf "Error in evaluating the expression for the variable %s" varKey)
                ))) .>> spaces

let parseTerm =
    [
        (parseNumericTerm); 
        (parseNumArray);
        (parseBoolStringAsDouble);
        (parsePrefixedUnaryOpTerm);
        (parseBracketedExpression)
        (parseQuotedString);
        (parseVariableTerm)
    ]
    |> choice
    <?> "term"


let parseContinuousTerms= 
    //Spaces are handled here
    spaces >>? ((parseTerm) .>>. (many ((opt(spaces >>? parseArithmeticOp .>> spaces)) .>>. ((parseTerm) <?> "term after operator")))) .>> spaces
    |>> convertContinuousTermsToSingleExpression


let ternaryExpression =
    //Spaces are not handled here, but handled in the dependent (parseContinuousTerms)
    parseContinuousTerms .>>? pchar '?' .>>. parseContinuousTerms .>> pchar ':' .>>. parseContinuousTerms
    |>> (fun ((ExpressionOutput condition,ExpressionOutput trueExp),ExpressionOutput falseExp) -> ExpressionOutput (TernaryExpression (condition, trueExp, falseExp)))
    <?> "ternary expression"

let parseExpression=
      //Spaces are not handled here, so every dependent must handle spaces
      (ternaryExpression <|> (parseContinuousTerms))
      <?> "Expression"

//Setting the forward referenced parsers after their definition
globalTermPerserRef := parseTerm
globalExpParserRef := parseExpression

let getParsedOutput inputString (variableNameBeingEvaluated) variablesDict =
    runParserOnString (((initializeVariablesDict variablesDict) >>. (setVariableNameToUserState variableNameBeingEvaluated) >>. (opt (pchar '=')) >>. parseExpression) .>> eof) UserState.Default "mainStream" inputString

let parseAndEvaluateExpressionExpressively (expressionString) (variablesDict) (variableNameBeingEvaluated) =
   let parsedExpression = getParsedOutput expressionString variableNameBeingEvaluated variablesDict
   match parsedExpression with
   | Success (expReturnType, userState, pos) ->
        match expReturnType with
        | ExpressionOutput (Expression.NumArray numarray) ->
            (EvaluationFailure (ArrayTypeNotSupportedAsReturnType "Expression returns Numeric Array type which is not a supported return type"), (""), Seq.ofList [])
        | ExpressionOutput (expr) ->
            ((EvaluateExpression (Some expr)), "", Seq.ofList (List.distinct (userState.VariablesReferenced @ userState.VariablesReferencedByThisLayer)))
   | Failure (label, err, pos) ->
        (EvaluationFailure (ParsingError (MathExpressionParsingFailureType.UnhandledInput ("\n" + label))) , err.ToString() , Seq.ofList [])
           

let refractoredImplExamples = fun() ->
    let variables = dict [
        "variableA", "1";
        "variableB", "2";
        "variableC", "variableB * variableB"
        ]

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
            .Add("2 + 2(4)", 10.)
            .Add("(1 + 2)(1 + 4)", 15.)
            .Add("asin 0", infinity)
            .Add("2*-2", -4.)

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
            .Add("2 2", ExpressionEvaluationError.ParsingError (MathExpressionParsingFailureType.IncompleteParsing ""))
            .Add("2 sin(3.143)", ExpressionEvaluationError.ParsingError (MathExpressionParsingFailureType.UnexpectedToken "Cannot allow this because we would be allowing to treat \"2 2\" as 2 * 2"))

    let multiLineInputs =
        [
            "2 + 2\n" + "3 + 4"
            "2 + 1\n +" + "3 + 2"
        ]

    let errorMessagesImprovementNeededInputs =
        [
            "(6 + c)";
            "2 + (5 * 2";
            "sin a"
        ]

    printfn "\n\n\nRefractoredImpl\n\n"
    let printParsedOutput inputString =
        printfn "Original Expression : %A" inputString
        printfn "%A" (getParsedOutput inputString "someUniqueName")

    let mutable countOfFailed = 0
    let mutable countOfSuccess = 0

    let printAllCases (key:string) (expectedValue: double) =
        let evaluatedResult = parseAndEvaluateExpressionExpressively key variables "someUniqueName"
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
                let isResultSuccess = 
                    (expectedValue.ToString()) = (getStringValueOfEvaluationResultType evaluatedValue)
                printfn "Result : %A" (if isResultSuccess then "Success" else "Failure")
                if isResultSuccess then
                    countOfSuccess <- countOfSuccess + 1
                else
                    countOfFailed <- countOfFailed + 1

    successTestCases |> Map.toSeq |> Seq.map fst |> Seq.iter printParsedOutput
    successTestCases |> Map.iter printAllCases
    printfn "\n\nNumber of failed cases : %A " countOfFailed
    printfn "Number of success cases : %A " countOfSuccess
    errorCases |> Map.toSeq |> Seq.map fst |> Seq.iter printParsedOutput
    errorCases |> Map.toSeq |> Seq.map fst |> Seq.iter (fun s -> printfn "ExpressionInput: %A\nEvaluatedOutput: %A" (s) (parseAndEvaluateExpressionExpressively s variables "someUniqueName"))
    printfn "\nMultiLineInputs :"
    multiLineInputs |> List.iter (fun s -> printfn "ExpressionInput: %A\nEvaluatedOutput: %A" (s) (parseAndEvaluateExpressionExpressively s variables "someUniqueName"))
    errorMessagesImprovementNeededInputs |> List.iter (fun s -> printfn "ExpressionInput: %A\nEvaluatedOutput: %A" (s) (parseAndEvaluateExpressionExpressively s variables "someUniqueName"))
    printfn "\nCustom cases : "
    let customCases = 
        [
        "sin 0";
        "cos 0";
        "cos 0.5";
        "ceil cos 0.5";
        "floor cos 0.5";
        "cos 0.5 * 2";
        "cos 1.0"
        ]
    customCases |> List.iter (fun s -> printfn "ExpressionInput: %A\nEvaluatedOutput: %A" (s) (parseAndEvaluateExpressionExpressively s variables "someUniqueName"))

    printfn "\n\nVariables test cases : "
    let variableTestCases =
        dict["firstVar","variableA + variableB"; "secondVar","variableC + variableA"]
    variableTestCases |> Seq.iter (fun s -> printfn "ExpressionInput: %A\nEvaluatedOutput: %A" (s.Value) (parseAndEvaluateExpressionExpressively (s.Value) variables (s.Key)))