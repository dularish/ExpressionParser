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
    | Constant of int
    | BinaryExpression of Expression*BinaryOperator*Expression
    | UnaryExpression of UnaryOperator * Expression

type Token = 
    | Constant of int
    | BinaryOperator of BinaryOperator
    | Bracket of Brackets

let intToUnion input =
    Token.Constant input

let parseConstant = 
    many1 parseDigit 
    |>> (fun (charList) -> String(List.toArray(charList)) |> int |> intToUnion)

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
    parseSpaces >>. (parseOpenBracket <|> parseCloseBracket <|> parseArithmeticOp <|> parseConstant)

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
        let (resultToEnd, finalRemaining) = (tryParseMathExpression remaining (stackExp) (Some opMatched))
        (resultToEnd, finalRemaining)
    | Success (Token.Constant constMatched, remaining) ->
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
                if (getPriority opMatched) > (getPriority stackOp.Value) then
                    let resultToEnd, remainingCase1 = tryParseMathExpression remainingCase1 (Some (Expression.Constant constMatched)) (Some opMatched)
                    (Some (Expression.BinaryExpression (someStackExp, stackOp.Value, resultToEnd.Value)), remainingCase1)
                else
                    let resultToEnd, remainingCase2 = tryParseMathExpression remainingCase1 (Some (Expression.BinaryExpression (someStackExp, stackOp.Value, Expression.Constant constMatched))) (Some opMatched)
                    (resultToEnd, remainingCase2)
            | Success (Token.Bracket BracketClose, remainingCase2) ->
                match stackOp with
                | Some someStackOp ->
                    tryParseMathExpression remainingCase2 (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched))) (None)
                | _ -> 
                    (None, input)
            | Failure _ ->
                match stackOp with
                | Some someStackOp ->
                    (Some (Expression.BinaryExpression (someStackExp, someStackOp, Expression.Constant constMatched)), remaining)
                | None ->
                    (stackExp, input)//Unexpected code block during proper input
            | _ ->
                (None, input)//Unexpected code block during proper input
    | Failure (_) ->
        (stackExp,input)

 
let listPatternMatching =
    match [1;2;3;4] with
    | first::second::someList::someList2 ->
        printfn "Multiple matching possible %A %A %A %A" first second someList someList2
        0
    | _ ->
        printfn "No, not possible" 
        0

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

    let testCases = new Map<string,double>()
    testCases.Add("2 + 3",5)
    testCases.Add("2 * 3",6)
    testCases.Add("89",89)
    testCases.Add("   12        -  8   ", 4)
    testCases.Add("142        -9   ", 133)
    testCases.Add("72+  15", 87)
    testCases.Add(" 12*  4", 48)
    testCases.Add(" 50/10", 5)
    testCases.Add("2.5", 2.5)
    testCases.Add("4*2.5 + 8.5+1.5 / 3.0", 19)
    testCases.Add("5.0005 + 0.0095", 5.01)
    testCases.Add("67+2", 69)
    testCases.Add(" 2-7", -5)
    testCases.Add("5*7 ", 35)
    testCases.Add("8/4", 2)
    testCases.Add("2 -4 +6 -1 -1- 0 +8", 10)
    testCases.Add("1 -1   + 2   - 2   +  4 - 4 +    6", 6)
    testCases.Add("2 -4 +6 -1 -1- 0 +8", 10)
    testCases.Add("1 -1   + 2   - 2   +  4 - 4 +    6", 6)
    testCases.Add(" 2*3 - 4*5 + 6/3 ", -12)
    testCases.Add("2*3*4/8 -   5/2*4 +  6 + 0/3   ", -1)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)
    //testCases.Add("",)

    let resultForParseAToken = ((many1 parseAToken) |> run) exp6
    let parsedExpression exp = tryParseExpression exp
    let parsedExpression2 exp = tryParseMathExpression exp (None) (None)

    let listOfExpressions = [exp1;exp2;exp3;exp4;exp5;exp6;exp7;exp8;exp9]
    //let listOfExpressions = [exp5]

    let printResult exp = printfn "Original Expression :\n%A\nParsed Expression :\n%A" exp (parsedExpression2 exp)
    listPatternMatching |> ignore
    listOfExpressions |> List.iter printResult