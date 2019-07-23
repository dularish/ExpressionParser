module MathematicalExpressionParser
open System
open ParserBuildingBlocks

type BinaryOperator =
    | Plus
    | Minus
    | Divide
    | Multiply

type UnaryOperator = 
    | Decrement
    | Increment
    | Log10

type Operator =
    | BinaryOperator of BinaryOperator
    | UnaryOperator of UnaryOperator

type Brackets =
    | BracketOpen
    | BracketClose

type Expression = 
    | Constant of double
    | BinaryExpression of Expression*BinaryOperator*Expression
    | UnaryExpression of UnaryOperator * Expression

type Token = 
    //| IntConstant of int
    | DoubleConstant of double
    | BinaryOperator of BinaryOperator
    | Bracket of Brackets

//let intToUnion input =
    //Token.IntConstant input

let doubleToUnion input =
    Token.DoubleConstant input

//let parseConstant = 
//    many1 parseDigit 
//    |>> (fun (charList) -> String(List.toArray(charList)) |> int |> intToUnion)

let parseDouble =
    (many1 parseDigit) .>>. (opt ((pChar '.') .>>. (many1 parseDigit) ))
    |>> (fun (wholeNums, decimalPart) ->
        match decimalPart with
        | Some (decimalPoint, decimalPartDigits) ->
            String(List.toArray(wholeNums @ [decimalPoint] @ decimalPartDigits))
            |> double |> doubleToUnion
        | None ->
            String(List.toArray(wholeNums)) |> double |> doubleToUnion)

let arithmeticOps = 
        ['+';'-';'/';'*']

let arithmeticCharToUnion input =
    if input = '+' then (BinaryOperator (Plus))
    elif input = '-' then (BinaryOperator (Minus))
    elif input = '*' then (BinaryOperator (Multiply))
    else (BinaryOperator (Divide))

let parseArithmeticOp =
    arithmeticOps
    |> anyOf
    |>> arithmeticCharToUnion

let brackets = 
    ['(';')']

let parseBrackets inputString= 
    let result = run (brackets |> anyOf) inputString
    match result with
    | Success ('(', remainingText1) ->
        Success (BracketOpen, remainingText1)
    | Success (')', remainingText2) ->
        Success (BracketClose, remainingText2)
    | Success (_,_) ->
        Failure "some error"
    | Failure err -> Failure err


let parseCloseBracket = pChar ')' |>> fun(_) -> Token.Bracket BracketClose
let parseOpenBracket = pChar '(' |>> fun(_) -> Token.Bracket BracketOpen

let parseSpaces = many (pChar ' ') 
let parseAToken = 
    parseSpaces >>. (parseOpenBracket <|> parseCloseBracket <|> parseArithmeticOp <|> parseDouble)

let parseTwoTokens =
    (parseAToken .>>. parseAToken)

type EntryType = 
    | Token of Token
    | TokenList of EntryType list

//This is a simple expression parser which doesn't solve by BODMAS
let rec tryParseExpression input =
    let result = run parseAToken input
    match result with
    | Success (Token.Bracket BracketClose, remaining) ->
        ([],remaining)
    | Success (Token.Bracket BracketOpen, remaining) ->
        let (resultToEndOfBracket, finalRemaining) = tryParseExpression remaining
        let oneFullExpression = EntryType.TokenList ( resultToEndOfBracket)
        let resultToOtherEnd, anotherFinalRemaining = tryParseExpression finalRemaining
        (oneFullExpression :: resultToOtherEnd, anotherFinalRemaining)
    | Success (otherToken, remaining) ->
        let (resultToEnd, finalRemaining) = (tryParseExpression remaining)
        ((EntryType.Token otherToken :: resultToEnd), finalRemaining)
    | Failure (_) ->
        ([],input)


let getPriority operator =
    if (operator = (BinaryOperator.Plus)) then 1
    elif (operator = (BinaryOperator.Minus)) then 1
    elif (operator = (BinaryOperator.Multiply)) then 2
    elif (operator = (BinaryOperator.Divide)) then 2
    else 0

//This implementation is a Mathematical expression parser which parses by applying BODMAS
let rec tryParseMathExpression input (stackExp:Expression option) (stackOp: BinaryOperator option)=
    let result = run parseAToken input
    match result with
    | Success (Token.Bracket BracketClose, remaining) ->
        //Validate whether stackOp is None - Pending
        (stackExp,remaining)
    | Success (Token.Bracket BracketOpen, remaining) ->
        let (resultToEndOfBracket, finalRemaining) = tryParseMathExpression remaining (None) (None)
        match resultToEndOfBracket with
        | Some someResultToOtherEnd ->
            match stackExp with
            | None ->
                (resultToEndOfBracket, finalRemaining)
            | Some someStackExp ->
                match stackOp with
                | None ->
                    (None, finalRemaining)//This is not expected during valid formula entry
                | Some someStackOp ->
                    (Some (Expression.BinaryExpression (someStackExp, someStackOp, someResultToOtherEnd)), finalRemaining)
        | None ->
            (stackExp, finalRemaining)//This is not expected when user enters multiplication even before brackets
    | Success (Token.BinaryOperator opMatched, remaining) ->
        match opMatched with
        | Minus ->
            match stackExp with
            | None ->
                match stackOp with
                | None ->
                    let subResult = run parseAToken remaining
                    match subResult with
                    | Success (Token.DoubleConstant someConstant, remainingSub) ->
                        let newStackExp = Some (Expression.Constant (someConstant * -1.0))
                        (tryParseMathExpression remainingSub (newStackExp) (None))
                    | Failure _ ->
                        (None , input) //This case when minus symbol is not followed by any token
                    | _ ->
                        (None, input) //This case when minus symbole is followed by a non-digit
                | _ ->
                    (None, input) // When a operator is matched stackOp should have been empty
            | Some someStackExp ->
                match stackOp with
                | None ->
                    (tryParseMathExpression remaining (stackExp) (Some opMatched))
                | Some _ ->
                    (None, input) //When an operator is matched, when stackExp is non-empty, stackOp should have been empty
        | _ ->
            //More cases of check can be better
            (tryParseMathExpression remaining (stackExp) (Some opMatched))
    | Success (Token.DoubleConstant constMatched, remaining) ->
        match stackExp with
        | None ->
            let (resultToEnd, finalRemaining) = (tryParseMathExpression remaining (Some (Expression.Constant constMatched)) (None))
            (resultToEnd, finalRemaining)
        | Some someStackExp ->
            let constMatchedCaseResult = run parseAToken remaining
            match constMatchedCaseResult with
            | Success (Token.BinaryOperator opMatched, remainingCase1) ->
                //Compare the priorities
                //If matched priority is higher then stackExp operator (tryParseMathExpression)
                //Otherwise tryParseMathExpression remainingCase1 (Expression of stackExp stackOp constMatched) (opMatched)

                match stackOp with
                | Some someStackOp ->
                    if (getPriority opMatched) > (getPriority someStackOp) then
                        let resultToEnd, remainingCase11 = tryParseMathExpression remainingCase1 (Some (Expression.Constant constMatched)) (Some opMatched)
                        match resultToEnd with
                        |Some someResultToEnd ->
                            //This block is written to handle the case "2*3 - 4*5 + 6/3" which without this block would get evaluated to "2*3 - (4*5 + 6/3)
                            match someResultToEnd with
                            | BinaryExpression (innerBinExpOperand1, innerBinExpOperator, innerBinExpOperand2) ->
                                let firstExpressionFormed = BinaryExpression (someStackExp, someStackOp, innerBinExpOperand1)
                                (Some (Expression.BinaryExpression (firstExpressionFormed, innerBinExpOperator, innerBinExpOperand2)), remainingCase11)
                            | Constant innerDoubleValue ->
                                (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant innerDoubleValue)), remainingCase11)
                        | None ->
                            (None, input) //Problem with evaluating remainder of expression but now the program doesn't know what to do with the some stackExp and some stackOp
                    else
                        let resultToEnd, remainingCase12 = tryParseMathExpression remainingCase1 (None) (None)
                        match resultToEnd with
                        |Some someResultToEnd ->
                            (Some (Expression.BinaryExpression (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched) , opMatched, someResultToEnd)), remainingCase12)
                        | None ->
                            (None, input) //Problem with evaluating remainder of expression but now the program doesn't know what to do with the some stackExp and some stackOp
                        //(Some (Expression.BinaryExpression (someStackExp, someStackOp, )))
                        //let resultToEnd, remainingCase12 = tryParseMathExpression remainingCase1 (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched))) (Some opMatched)
                        //(resultToEnd, remainingCase12)
                | None ->
                    (None, input) //Unexpected code during proper input This is possible during negative number input//When a constant is matched there should either have been (some stackExp and some stackOp) or (None stackExp and None stackOp)//The first either or case fails here
                    
            | Success (Token.Bracket BracketClose, remainingCase2) ->
                match stackOp with
                | Some someStackOp ->
                    tryParseMathExpression remainingCase2 (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched))) (None)
                | _ -> 
                    (None, input)//Empty expressions
            | Failure _ ->
                match stackOp with
                | Some someStackOp ->
                    (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched)), remaining) //This could be the end of expression
                | None ->
                    (stackExp, input)//Just returning the stackExp, but the program doesn't know what to do with the the parsed constMatched
            | _ ->
                (None, input)//Unhandled cases viz UnaryExpressions, InvalidInput
    | Failure (_) ->
        (stackExp,input)//Every expression is designed to terminate from here or from any of the None lines



//This implementation is a Mathematical expression parser which parses by applying BODMAS
let rec tryParseMathExpressionWithMemoryOp input (stackExp:Expression option) (stackOp: BinaryOperator option) (memoryOp: BinaryOperator option)=
    let result = run parseAToken input
    match result with
    | Success (Token.Bracket BracketClose, remaining) ->
        //Validate whether stackOp is None - Pending
        (stackExp,remaining)
    | Success (Token.Bracket BracketOpen, remaining) ->
        let (resultToEndOfBracket, finalRemaining) = tryParseMathExpressionWithMemoryOp remaining (None) (None) (memoryOp)
        match resultToEndOfBracket with
        | Some someResultToOtherEnd ->
            match stackExp with
            | None ->
                (resultToEndOfBracket, finalRemaining)
            | Some someStackExp ->
                match stackOp with
                | None ->
                    (None, finalRemaining)//This is not expected during valid formula entry
                | Some someStackOp ->
                    (Some (Expression.BinaryExpression (someStackExp, someStackOp, someResultToOtherEnd)), finalRemaining)
        | None ->
            (stackExp, finalRemaining)//This is not expected when user enters multiplication even before brackets
    | Success (Token.BinaryOperator opMatched, remaining) ->
        let opMatchedHandlingWithoutMemory = fun() ->
            match opMatched with
            | Minus ->
                match stackExp with
                | None ->
                    match stackOp with
                    | None ->
                        let subResult = run parseAToken remaining
                        match subResult with
                        | Success (Token.DoubleConstant someConstant, remainingSub) ->
                            let newStackExp = Some (Expression.Constant (someConstant * -1.0))
                            (tryParseMathExpressionWithMemoryOp remainingSub (newStackExp) (None) (memoryOp))
                        | Failure _ ->
                            (None , input) //This case when minus symbol is not followed by any token
                        | _ ->
                            (None, input) //This case when minus symbole is followed by a non-digit
                    | _ ->
                        (None, input) // When a operator is matched stackOp should have been empty
                | Some someStackExp ->
                    match stackOp with
                    | None ->
                        (tryParseMathExpressionWithMemoryOp remaining (stackExp) (Some opMatched) (memoryOp))
                    | Some _ ->
                        (None, input) //When an operator is matched, when stackExp is non-empty, stackOp should have been empty
            | _ ->
                //More cases of check can be better
                (tryParseMathExpressionWithMemoryOp remaining (stackExp) (Some opMatched) (memoryOp))

        match memoryOp with
        | Some someMemoryOp ->
            if getPriority opMatched <= getPriority someMemoryOp then
                (stackExp, input)
            else 
                opMatchedHandlingWithoutMemory()
        | None ->
            opMatchedHandlingWithoutMemory()
    | Success (Token.DoubleConstant constMatched, remaining) ->
        match stackExp with
        | None ->
            let (resultToEnd, finalRemaining) = (tryParseMathExpressionWithMemoryOp remaining (Some (Expression.Constant constMatched)) (None) (memoryOp))
            (resultToEnd, finalRemaining)
        | Some someStackExp ->
            let constMatchedCaseResult = run parseAToken remaining
            match constMatchedCaseResult with
            | Success (Token.BinaryOperator opMatched, remainingCase1) ->
                //Compare the priorities
                //If matched priority is higher then stackExp operator (tryParseMathExpression)
                //Otherwise tryParseMathExpression remainingCase1 (Expression of stackExp stackOp constMatched) (opMatched)

                match (stackOp) with
                | Some someStackOp ->
                    if (getPriority opMatched) > (getPriority someStackOp) then
                        let resultToEnd, remainingCase11 = tryParseMathExpressionWithMemoryOp remainingCase1 (Some (Expression.Constant constMatched)) (Some opMatched) (stackOp)
                        match resultToEnd with
                        |Some someResultToEnd ->
                            tryParseMathExpressionWithMemoryOp remainingCase11 (Some (Expression.BinaryExpression (someStackExp, someStackOp, someResultToEnd))) (None) (memoryOp)
                        | None ->
                            (None, input) //Problem with evaluating remainder of expression but now the program doesn't know what to do with the some stackExp and some stackOp
                    else
                        let constMatchHandlingWithoutMemoryOp = fun() ->
                            let resultToEnd, remainingCase12 = tryParseMathExpressionWithMemoryOp remainingCase1 (None) (None) (Some opMatched)
                            match resultToEnd with
                            |Some someResultToEnd ->
                                tryParseMathExpressionWithMemoryOp remainingCase12 (Some(Expression.BinaryExpression (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched) , opMatched, someResultToEnd))) (None) (memoryOp)
                            | None ->
                                (None, input) //Problem with evaluating remainder of expression but now the program doesn't know what to do with the some stackExp and some stackOp
                            //(Some (Expression.BinaryExpression (someStackExp, someStackOp, )))
                            //let resultToEnd, remainingCase12 = tryParseMathExpression remainingCase1 (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched))) (Some opMatched)
                            //(resultToEnd, remainingCase12)
                        match memoryOp with
                        | Some someMemoryOp ->
                            if (getPriority opMatched > getPriority someMemoryOp) then
                                constMatchHandlingWithoutMemoryOp()
                            else
                                (Some(BinaryExpression (someStackExp, someStackOp, Constant constMatched)),remaining)
                        | None ->
                            constMatchHandlingWithoutMemoryOp()
                | None ->
                    (None, input) //Unexpected code during proper input This is possible during negative number input//When a constant is matched there should either have been (some stackExp and some stackOp) or (None stackExp and None stackOp)//The first either or case fails here
                    
            | Success (Token.Bracket BracketClose, remainingCase2) ->
                match stackOp with
                | Some someStackOp ->
                    tryParseMathExpressionWithMemoryOp remainingCase2 (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched))) (None) (memoryOp)
                | _ -> 
                    (None, input)//Empty expressions
            | Failure _ ->
                match stackOp with
                | Some someStackOp ->
                    (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched)), remaining) //This could be the end of expression
                | None ->
                    (stackExp, input)//Just returning the stackExp, but the program doesn't know what to do with the the parsed constMatched
            | _ ->
                (None, input)//Unhandled cases viz UnaryExpressions, InvalidInput
    | Failure (_) ->
        (stackExp,input)//Every expression is designed to terminate from here or from any of the None lines

 
let listPatternMatching =
    match [1;2;3;4] with
    | first::second::someList::someList2 ->
        printfn "Multiple matching possible %A %A %A %A" first second someList someList2
        0
    | _ ->
        printfn "No, not possible" 
        0



type ExpressionEvaluationError =
    | UnexpectedToken of string
    | InvalidOperatorUse of string
    | UnRecognizedToken of string
    | DivideByZeroAttempted of string
    | UnBalancedParanthesis of string

type ExpressionEvaluationResult =
    | EvaluationSuccess of double
    | EvaluationFailure of ExpressionEvaluationError

let computeBinaryExpression operand1 operator operand2 =
    match operator with
    | Plus ->
        EvaluationSuccess (operand1 + operand2)
    | Minus ->
        EvaluationSuccess (operand1 - operand2)
    | Multiply ->
        EvaluationSuccess (operand1 * operand2)
    | Divide ->
        if operand2 = 0. then
            EvaluationFailure (DivideByZeroAttempted ("Expression tried to evaluate : " + operand1.ToString() + operator.ToString() + operand2.ToString()))
        else
            EvaluationSuccess (operand1 / operand2)

let rec EvaluateExpression (exp: Expression option) =
    match exp with
    | Some someExp ->
        match someExp with
        | Constant doubleConstant ->
            EvaluationSuccess doubleConstant
        | BinaryExpression (exp1, operator, exp2) ->
            let exp1Result = EvaluateExpression (Some exp1)
            let exp2Result = EvaluateExpression (Some exp2)
            match (exp1Result, exp2Result) with
            | (EvaluationSuccess exp1Dbl, EvaluationSuccess exp2Dbl) ->
                computeBinaryExpression exp1Dbl operator exp2Dbl
            | (EvaluationFailure failure, _) ->
                EvaluationFailure failure
            | (_ , EvaluationFailure failure) ->
                EvaluationFailure failure
        | _ ->
            EvaluationFailure (UnRecognizedToken "NotImplemented")
    | None ->
        EvaluationFailure (UnRecognizedToken "Null expression - Not expected")

let parseAndEvaluateExpression (expressionString) = 
    let parsedExpression = tryParseMathExpressionWithMemoryOp expressionString (None) (None) (None)
    parsedExpression
    |> fun(exp, remainingString) ->
        ((EvaluateExpression exp), remainingString)

let examplesForMathematicalExpressionParser =
    let exp1 = "23"
    let exp2 = "23 + 42"
    let exp3 = "(23 + 42)"
    let exp4 = "(23 + (22 + 43))"
    let exp5 = "(23 + 22) + 53"
    let exp6 = "((23 + 22) + 53)"
    let exp7 = "(23 + 24) + (25 + 26)"
    let exp8 = "21 + 22 + 23 + 24 + 25 + 26"
    let exp9 = "1 + 2 * 3 + 5 + 6 * 8"
    let exp10 = "1 - 2 * 3"
    let exp11 = "1*(-3)"

    let successTestCasesResults = 
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
            //Region end - TestcasesCreated by me
            //Region begin - TestCases from 
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
            //End region - Testcases from

    let errorCases = 
        Map.empty
            .Add("  6 + c", ExpressionEvaluationError.UnRecognizedToken)
            .Add("  7 & 2", ExpressionEvaluationError.InvalidOperatorUse)
            .Add("  %", ExpressionEvaluationError.UnexpectedToken)
            .Add(" 5 + + 6", ExpressionEvaluationError.InvalidOperatorUse)
            .Add("5/0", ExpressionEvaluationError.DivideByZeroAttempted)
            .Add(" 2 - 1 + 14/0 + 7", ExpressionEvaluationError.DivideByZeroAttempted)
            .Add("(5*7/5) + (23) - 5 * (98-4)/(6*7-42)", ExpressionEvaluationError.DivideByZeroAttempted)
            .Add("2 + (5 * 2", ExpressionEvaluationError.UnBalancedParanthesis)
            .Add("(((((4))))", ExpressionEvaluationError.UnBalancedParanthesis)
            .Add("((2)) * ((3", ExpressionEvaluationError.UnBalancedParanthesis)
            .Add("((9)) * ((1)", ExpressionEvaluationError.UnBalancedParanthesis)

    let resultForParseAToken = ((many1 parseAToken) |> run) exp6
    let parsedExpression exp = tryParseExpression exp
    let parsedExpression2 exp = tryParseMathExpressionWithMemoryOp exp (None) (None) (None)

    //let listOfExpressions = [exp1;exp2;exp3;exp4;exp5;exp6;exp7;exp8;exp9;exp10;exp11]
    let listOfExpressions = ["(5 + 2*3 - 1 + 7 * 8)"]
    //let listOfExpressions = ["21 + 22 + 23 + 24 + 25 + 26"]

    let printResult exp = printfn "Original Expression :\n%A\nParsed Expression :\n%A\nEvaluated Result :\n%A\n" exp (parsedExpression2 exp) ((parsedExpression2 exp)|> (fun (exp, remainingString) -> (EvaluateExpression exp)))
    listPatternMatching |> ignore
    listOfExpressions |> List.iter printResult
    let mutable countOfFailed = 0
    let printFailureCase (key:string) (expectedValue: double) =
        let evaluatedResult = parseAndEvaluateExpression key
        match evaluatedResult with
        | EvaluationFailure someString, _ ->
            printfn "Failure to evaluate %s" key
            printfn "\nEvaluation Failure message : %A" someString
        | EvaluationSuccess evaluatedValue , remaining ->
            if remaining.Trim().Length > 0 then
                printfn "The following expression evaluated successfully but some text remaining"
                printfn "Expression : %A" key
                printfn "Remaining String : %A" remaining

            if evaluatedValue <> expectedValue then
                printfn "The following expression does not evaluate to the expected value :"
                printfn "Expression : %A" key
                printfn "Expected value : %A" expectedValue
                printfn "Obtained value : %A" evaluatedValue
                countOfFailed <- countOfFailed + 1

    successTestCasesResults |> Map.iter printFailureCase

    printfn "\n\nNumber of failed cases : %A " countOfFailed
