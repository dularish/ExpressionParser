module MathematicalExpressionParser
open System
open ParserBuildingBlocks

type ArithmeticOperator =
    | Plus
    | Minus
    | Divide
    | Multiply

type Brackets =
    | BracketOpen
    | BracketClose

type Expression = 
    | Constant of int
    | Expression of Expression*ArithmeticOperator*Expression

type Token = 
    | Constant of int
    | ArithmeticOperator of ArithmeticOperator
    | Bracket of Brackets

let intToUnion input =
    Token.Constant input

let parseConstant = 
    many1 parseDigit 
    |>> (fun (charList) -> String(List.toArray(charList)) |> int |> intToUnion)

let arithmeticOps = 
        ['+';'-';'/';'*']

let arithmeticCharToUnion input =
    if input = '+' then Token.ArithmeticOperator Plus
    else if input = '-' then Token.ArithmeticOperator Minus
    else if input = '*' then Token.ArithmeticOperator Multiply
    else Token.ArithmeticOperator Divide

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



//let rec parseExpression inputString = 
//    let resultForOpenBracket = run parseOpenBracket inputString

//    match resultForOpenBracket with 
//    | Success(_, remainingText) ->
//        let parsedInnerExp = parseExpression .>>. (opt (parseArithmeticOp .>>. parseExpression)) .>> parseCloseBracket
//        let resultForparsedInnerExp = run parsedInnerExp remainingText
//        match resultForparsedInnerExp with 
//        | Success((exp*opt*exp), ArithmeticOperator) ->

let parseSpaces = many (pChar ' ') 
let parseAToken = 
    parseSpaces >>. (parseOpenBracket <|> parseCloseBracket <|> parseArithmeticOp <|> parseConstant)

type EntryType = 
    | Token of Token
    | TokenList of EntryType list

let rec tryParseExpression input =
    let result = run parseAToken input
    match result with
    | Success (Token.Bracket BracketClose, remaining) ->
        ([EntryType.Token (Bracket BracketClose)],remaining)
    | Success (Token.Bracket BracketOpen, remaining) ->
        let (resultToEndOfBracket, finalRemaining) = tryParseExpression remaining
        let oneFullExpression = EntryType.TokenList ( (EntryType.Token (Bracket BracketOpen)) :: resultToEndOfBracket)
        let resultToOtherEnd, anotherFinalRemaining = tryParseExpression finalRemaining
        (oneFullExpression :: resultToOtherEnd, anotherFinalRemaining)
    | Success (otherToken, remaining) ->
        let (resultToEnd, finalRemaining) = (tryParseExpression remaining)
        ((EntryType.Token otherToken :: resultToEnd), finalRemaining)
    | Failure (_) ->
        ([],input)

let evaluateCalculation firstNum (operator:Token.ArithmeticOperator) secondNum =
    

let rec evaluateExpression (inputList:EntryType list) =
    match inputList with
    | [] ->
        Token.Constant 0
    | [EntryType.TokenList oneToken] ->
        evaluateExpression oneToken
    | [EntryType.Token oneToken] ->
        oneToken
    | (EntryType.TokenList firstToken) :: someTail ->
        let firstTokenResult = evaluateExpression firstToken
        evaluateExpression (EntryType.Token(firstTokenResult) :: someTail)
    | (EntryType.Token firstToken) :: (EntryType.TokenList secondTokenList) :: someList ->
        let secondTokenResult = evaluateExpression secondTokenList
        evaluateExpression (EntryType.Token(firstToken) :: EntryType.Token(secondTokenResult) :: someList)
    | (EntryType.Token firstToken) :: (EntryType.Token secondToken) :: (EntryType.TokenList thirdTokenList) :: someList ->
        let thirdTokenResult = evaluateExpression thirdTokenList
        evaluateExpression (EntryType.Token(firstToken) :: EntryType.Token(secondToken) :: EntryType.Token(thirdTokenResult) :: someList)
    | [EntryType.Token firstToken;EntryType.Token (Token.ArithmeticOperator secondToken);EntryType.Token thirdToken] ->
        
 
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
    let exp6 = "(23 + 24) + (25 + 26)"

    let resultForParseAToken = ((many1 parseAToken) |> run) exp6
    let resultForExpression exp = tryParseExpression exp

    let listOfExpressions = [exp1;exp2;exp3;exp4;exp5;exp6]

    let printResult exp = printfn "%A" (resultForExpression exp)
    listPatternMatching |> ignore
    listOfExpressions |> List.iter printResult
