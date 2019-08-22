module MathematicalExpressionParser
open System
open ParserBuildingBlocks
open System.Collections.Generic

let mutable variables = dict [
                "variableA", "1";
                "variableB", "2";
                "variableC", "variableB * variableB"
                ]

let masterVariables = dict [
                "pi", Math.PI.ToString();
                "e", Math.E.ToString()]

type BinaryOperator =
    | Plus
    | Minus
    | Divide
    | Multiply
    | Pow
    | Modulo

type UnaryOperator = 
    | Exp
    | Sin
    | Cos
    | Tan
    | ASin
    | ACos
    | ATan
    | Sinh
    | Cosh
    | Tanh
    | ASinh
    | ACosh
    | ATanh
    | Log
    | Ln
    | Floor
    | Ceil
    | Sqrt
    | Abs

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
    | DoubleConstant of double
    | BinaryOperator of BinaryOperator
    | UnaryOperator of UnaryOperator
    | Bracket of Brackets
    | Variable of string
    | MasterKeywordVariable of string

type StableToken =
    | Operator
    | Expression

let doubleToUnion input =
    Token.DoubleConstant input

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
        ['+';'-';'/';'*';'^';'%']

let unaryOps =
    ["exp";"sin";"cos";"tan";"acos";"asin";"atan";"sinh";"cosh";"tanh";"asinh";"acosh";"atanh";"log";"ln";"floor";"ceil";"sqrt";"abs"]

let arithmeticCharToUnion input =
    if input = '+' then (BinaryOperator (Plus))
    elif input = '-' then (BinaryOperator (Minus))
    elif input = '*' then (BinaryOperator (Multiply))
    elif input = '^' then (BinaryOperator (Pow))
    elif input = '%' then (BinaryOperator (Modulo))
    else (BinaryOperator (Divide))

let unaryStrToUnion input =
    if input = "exp" then (UnaryOperator (Exp))
    elif input = "sin" then (UnaryOperator (Sin))
    elif input = "cos" then (UnaryOperator (Cos))
    elif input = "tan" then (UnaryOperator (Tan))
    elif input = "asin" then (UnaryOperator (ASin))
    elif input = "acos" then (UnaryOperator (ACos))
    elif input = "atan" then (UnaryOperator (ATan))
    elif input = "sinh" then (UnaryOperator (Sinh))
    elif input = "cosh" then (UnaryOperator (Cosh))
    elif input = "tanh" then (UnaryOperator (Tanh))
    elif input = "asinh" then (UnaryOperator (ASinh))
    elif input = "acosh" then (UnaryOperator (ACosh))
    elif input = "atanh" then (UnaryOperator (ATanh))
    elif input = "log" then (UnaryOperator (Log))
    elif input = "ln" then (UnaryOperator (Ln))
    elif input = "floor" then (UnaryOperator (Floor))
    elif input = "ceil" then (UnaryOperator (Ceil))
    elif input = "sqrt" then (UnaryOperator (Sqrt))
    else (UnaryOperator (Abs))

let parseArithmeticOp =
    arithmeticOps
    |> anyOf
    |>> arithmeticCharToUnion

let parseUnaryOp =
    unaryOps
    |> Seq.sortByDescending (fun x -> x.Length)
    |> Seq.map (fun x -> pString x)
    |> List.ofSeq
    |> choice
    |>> unaryStrToUnion

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

let masterVariableNameToToken inputString =
    MasterKeywordVariable (inputString)

let parseMasterVariable =
    masterVariables.Keys
    |> Seq.sortByDescending (fun x -> x.Length)
    |> Seq.map (fun x -> pString x)
    |> List.ofSeq
    |> choice
    |>> masterVariableNameToToken

let variableNameToToken inputString =
    Variable (inputString)

let parseVariable = fun() ->
    variables.Keys
    |> Seq.sortByDescending (fun x -> x.Length)
    |> Seq.map (fun x -> pString x)
    |> List.ofSeq
    |> choice
    |>> variableNameToToken

let parseSpaces = many (pChar ' ') 
let parseAToken = fun() ->
    if variables.Count > 0 then
        parseSpaces >>. (parseOpenBracket <|> parseCloseBracket <|> parseUnaryOp <|> parseArithmeticOp <|> parseDouble <|> (parseVariable()) <|> parseMasterVariable)
    else
        parseSpaces >>. (parseOpenBracket <|> parseCloseBracket <|> parseArithmeticOp <|> parseDouble <|> parseMasterVariable)

let parseTwoTokens =
    (parseAToken() .>>. parseAToken())

let parseQuotedStringInnerValuesChoices =
    let alphabets =
        ['a'..'z'] @ ['A'..'Z']
        |> List.map (fun a -> a.ToString())
    alphabets @ ["\\\""]
    |> List.map (fun s -> pString s)
    |> choice

let parseQuotedString = 
    ((pChar '"') >>. (many parseQuotedStringInnerValuesChoices) .>> (pChar '"'))
    |>> List.reduce (+)

type EntryType = 
    | Token of Token
    | TokenList of EntryType list

let getPriority operator =
    match operator with
    | Plus -> 1
    | Minus -> 1
    | Multiply -> 2
    | Divide -> 2
    | Modulo -> 2
    | Pow -> 3

type Associativity =
    | Left
    | Right

let getAssociativity operator =
    match operator with
    | Pow ->
        Right   //Because DotNet evaluates this way, so conforming to DotNet
    | _ ->
        Left

type MathExpressionParsingFailureType =
    | InsufficientParanthesis of string
    | TooManyParanthesis of string
    | MissingOperator of string
    | EmptyParanthesis of string
    | TrailingNegativeSign of string
    | InvalidSymbolFollowedByNegativeSign of string
    | SequencingOfOperatorsNotAllowed of string
    | OperatorNotExpected of string
    | UnhandledInput of string
    | EmptyExpression of string
    | UnrecognizedInput of string
    | EmptyVariableExpression of string
    | VariableParsingFailed of string
    | VariableDoesNotExists of string
    | UnexpectedToken of string
    | InvalidExpressionTerm of string
    | InvalidNumberOfExpressionsInStack of string
    | NotEnoughOperands of string
    | TooManyOperands of string
    | CircularReferencingFound of string

type MathExpressionParserResult =
    | ExpressionParsingSuccess of Expression option
    | ExpressionParsingFailure of MathExpressionParsingFailureType


//This implementation is a Mathematical expression parser based on Shunting Yard algorithm
let rec tryParseMathExpressionByAppr2 input (stackExp:Expression list) (stackOp: BinaryOperator list) (openedBracketsCount:int) (refVariables:string list) (lastToken:StableToken option) (variablesComputationPath:string list)=
    
    //This helper function can be used to get the next expression term, the next expression term could be a double number,
        //a variable, a bracketed expression or an unary expression
    let rec getTheNextTermAsExpression = fun(expressionString:string) ->
        let nextToken = run (parseAToken()) expressionString
        match nextToken with
        | Success (Token.Bracket BracketOpen, remainingAfterBracketOpen) ->
            let resultToEndOfBracket, finalRemaining, numberOfBracketsOpenedInResult, resultVariablesRef = tryParseMathExpressionByAppr2 remainingAfterBracketOpen ([]) ([]) (1) [] None variablesComputationPath
            if numberOfBracketsOpenedInResult > 0 then
                (ExpressionParsingFailure (InsufficientParanthesis expressionString), expressionString, numberOfBracketsOpenedInResult , resultVariablesRef)
            elif numberOfBracketsOpenedInResult < 0 then
                (ExpressionParsingFailure (TooManyParanthesis expressionString), expressionString, numberOfBracketsOpenedInResult, resultVariablesRef)
            else
                match resultToEndOfBracket with
                | ExpressionParsingSuccess parsedResult ->
                    match parsedResult with
                    | Some someResultToOtherEnd ->
                        (ExpressionParsingSuccess (parsedResult), finalRemaining, numberOfBracketsOpenedInResult, resultVariablesRef)
                    | None ->
                        (ExpressionParsingFailure (EmptyExpression expressionString), finalRemaining, numberOfBracketsOpenedInResult, resultVariablesRef)
                | ExpressionParsingFailure failure ->
                    (ExpressionParsingFailure failure, expressionString, numberOfBracketsOpenedInResult, resultVariablesRef)
        | Success (Token.DoubleConstant doubleConstant, remainingAfterDoubleConstant) ->
            (ExpressionParsingSuccess (Some (Expression.Constant doubleConstant)), remainingAfterDoubleConstant, 0, [])
        | Success (Token.Variable variableName, remainingAfterVariable) ->
            if (variablesComputationPath |> List.contains variableName) then
                (ExpressionParsingFailure (CircularReferencingFound (sprintf "Circular referencing found in the evaluation as %A" (variableName :: variablesComputationPath))), expressionString, openedBracketsCount, [])
            else
                if variables.ContainsKey(variableName) then
                    let parserResult, remainingString, _, _ = tryParseMathExpressionByAppr2 variables.[variableName] ([]) ([]) (openedBracketsCount) [] None (variableName :: variablesComputationPath)
                    match parserResult with
                    | ExpressionParsingSuccess expOpt ->
                        match expOpt with
                        | Some expression ->
                            (ExpressionParsingSuccess (expOpt), remainingAfterVariable, 0, [variableName])
                        | None ->
                            (ExpressionParsingFailure (EmptyVariableExpression (sprintf "Variable %s does not have an expression" variableName)), expressionString, openedBracketsCount, [variableName])
                    | ExpressionParsingFailure failure ->
                        match failure with
                        | CircularReferencingFound failureMessage ->
                            (ExpressionParsingFailure failure, expressionString, openedBracketsCount, [])
                        | _ ->
                            (ExpressionParsingFailure (VariableParsingFailed (sprintf "Unable to parse the expression related to the variable %s" variableName)), expressionString, openedBracketsCount, [variableName])
                else
                    (ExpressionParsingFailure (VariableDoesNotExists (sprintf "Variable %s existed during parsing doesn't exists during evaluation" variableName)), expressionString, openedBracketsCount, refVariables)
        | Success (Token.MasterKeywordVariable masterKeywordVariable, remainingAfterMasterVariable) ->
            if masterVariables.ContainsKey(masterKeywordVariable) then
                let parserResult, remainingString, _, _ = tryParseMathExpressionByAppr2 masterVariables.[masterKeywordVariable] ([]) ([]) (0) [] None variablesComputationPath
                match parserResult with
                | ExpressionParsingSuccess expOpt ->
                    match expOpt with
                    | Some expression ->
                        (ExpressionParsingSuccess (expOpt), remainingAfterMasterVariable, 0, [])
                    | None ->
                        (ExpressionParsingFailure (EmptyVariableExpression (sprintf "Master Variable %s does not have an expression" masterKeywordVariable)), expressionString, openedBracketsCount, refVariables)
                | ExpressionParsingFailure failure ->
                    (ExpressionParsingFailure (VariableParsingFailed (sprintf "Unable to parse the expression related to the master variable %s" masterKeywordVariable)), expressionString, openedBracketsCount, refVariables)
            else
                (ExpressionParsingFailure (VariableDoesNotExists (sprintf "Master Variable %s existed during parsing doesn't exists during evaluation" masterKeywordVariable)), expressionString, openedBracketsCount, refVariables)
        | Success (Token.UnaryOperator unaryOp, remainingAfterUnaryOp) ->
            let expressionTermResult, remainingAfterExprTerm, _, refVariablesInUnaryExp = getTheNextTermAsExpression(remainingAfterUnaryOp)
            match expressionTermResult with
            | ExpressionParsingSuccess expOpt ->
                match expOpt with
                | Some expression ->
                    let expressionToReturn = UnaryExpression (unaryOp, expression)
                    (ExpressionParsingSuccess (Some expressionToReturn), remainingAfterExprTerm, 0, refVariablesInUnaryExp)
                | None ->
                    (ExpressionParsingFailure (EmptyVariableExpression (sprintf "Unary operator %A does not have a valid expression" unaryOp)), expressionString, openedBracketsCount, refVariables)
            | ExpressionParsingFailure failure ->
                (ExpressionParsingFailure (VariableParsingFailed (sprintf "Unable to parse the expression related to the Unary operator %A" unaryOp)), expressionString, openedBracketsCount, refVariables)
        | _ ->
            (ExpressionParsingFailure (InvalidExpressionTerm (sprintf "Unable to find a valid expression term in the input string : %A" expressionString)), expressionString, openedBracketsCount, refVariables)

    let result = run (parseAToken()) input
    match result with
    | Success (Token.Bracket BracketClose, remaining) ->
        //When it's a bracket close expression, we return the expression parsed till this bracket and let the caller handle the rest of the expression after this bracket close
            //The caller as per current implementation could only be the helper function
        if openedBracketsCount = 1 then
            let parsedResult, remainingStringAfterBracketClose, numberOfOpenedBrackets, _ = tryParseMathExpressionByAppr2 "" stackExp stackOp (openedBracketsCount - 1) refVariables (Some (StableToken.Expression)) variablesComputationPath
            match parsedResult with
            | ExpressionParsingSuccess parsedExpression ->
                match parsedExpression with
                | Some someParsedExpression ->
                    (ExpressionParsingSuccess (Some someParsedExpression), remaining, 0, refVariables)
                | _ ->
                    (ExpressionParsingFailure (EmptyExpression input), input, numberOfOpenedBrackets, refVariables)
            | _ ->
                (parsedResult, input, numberOfOpenedBrackets, refVariables)
        else
            (ExpressionParsingFailure (TooManyParanthesis input) , input, openedBracketsCount, refVariables)//Not changing the state of isOpened as it's no more possible to close
    | Success (Token.Bracket _, _)//This would definitely have to be OpenBracket
    | Success (Token.DoubleConstant _, _)
    | Success (Token.MasterKeywordVariable _, _)
    | Success (Token.Variable _, _)
    | Success (Token.UnaryOperator _, _) ->
        //All these matched types mean, that the next term can be converted to an expression with the help of the helper function that we have built
            //But the lastToken matched before must be either None or an operator, which means the current implementation doesn't support "(1+3)(2+4)" or "2(4)"
        match lastToken with
        | None
        | Some (StableToken.Operator) ->
            let (resultToEndOfBracket, finalRemaining, isResultOpened, resultVariablesRef) = getTheNextTermAsExpression(input)
            let totalVariablesRef = (refVariables @ resultVariablesRef) |> List.distinct
            match resultToEndOfBracket with
            | ExpressionParsingSuccess parsedResult ->
                match parsedResult with
                | Some someResultToOtherEnd ->
                    tryParseMathExpressionByAppr2 finalRemaining (someResultToOtherEnd :: stackExp) (stackOp) (openedBracketsCount) (totalVariablesRef) (Some (StableToken.Expression)) variablesComputationPath
                | None ->
                    (ExpressionParsingFailure (EmptyExpression input), finalRemaining, openedBracketsCount, totalVariablesRef)//This is not expected when user enters multiplication even before brackets
            | ExpressionParsingFailure failure ->
                (ExpressionParsingFailure failure, input, openedBracketsCount, totalVariablesRef)
        | Some (StableToken.Expression) ->
            (ExpressionParsingFailure (MissingOperator input), input, openedBracketsCount, refVariables)
    | Success (Token.BinaryOperator opMatched, remaining) ->
        //When an operator is matched,
            //if the matched operator's priority is greater that the top of the operatorstack,  it will be pushed to operator stack
                //otherwise top stack expressions would be evaluated with top stack operators till it is so
            //Last token that was matched before must be an expression, because sequencing of operators is not allowed
        match lastToken with
        | None ->
            match opMatched with
            | Minus ->
                //This block is because Minus coming at the start of the expression would negate the expression followed by it
                    //But the current implementation does not allow the use of Minus symbol in the middle of the expression like "2 ^ -2", the workaround is "2 ^ (-2)"
                let (exprTermResult, remainingAfterExpressionTerm, isResultOpened, resultVariablesRef) = getTheNextTermAsExpression(remaining)
                let totalVariablesRef = (refVariables @ resultVariablesRef) |> List.distinct
                match exprTermResult with
                | ExpressionParsingSuccess parsedResult ->
                    match parsedResult with
                    | Some someResultToOtherEnd ->
                        let expressionToPass = BinaryExpression ( Expression.Constant (-1.0), BinaryOperator.Multiply, someResultToOtherEnd) 
                        tryParseMathExpressionByAppr2 remainingAfterExpressionTerm (expressionToPass::stackExp) (stackOp) (openedBracketsCount) totalVariablesRef (Some (StableToken.Expression)) variablesComputationPath
                    | None ->
                        //Control to this point not expected, if it reaches this point, developer has to be know
                        (ExpressionParsingFailure (EmptyExpression remaining), input, openedBracketsCount, totalVariablesRef)
                | ExpressionParsingFailure failure ->
                    (ExpressionParsingFailure failure, remaining, openedBracketsCount, totalVariablesRef)
            | _ ->
                (ExpressionParsingFailure (OperatorNotExpected remaining), input, openedBracketsCount, refVariables)
        | Some (StableToken.Expression) ->
            match stackOp with
            | [] ->
                tryParseMathExpressionByAppr2 remaining (stackExp) (opMatched :: stackOp) (openedBracketsCount) refVariables (Some (StableToken.Operator)) variablesComputationPath
            | stackTopOp :: restOps ->
                if ((getPriority(opMatched) > getPriority(stackTopOp)) || (((opMatched = stackTopOp) && ((getAssociativity opMatched) = Right )))) then
                    tryParseMathExpressionByAppr2 remaining (stackExp) (opMatched :: stackOp) (openedBracketsCount) refVariables (Some (StableToken.Operator)) variablesComputationPath
                else
                    match stackExp with
                    | topMostExp :: secondTopExp :: restExps ->
                        let newExprToPush = BinaryExpression (secondTopExp, stackTopOp, topMostExp)
                        tryParseMathExpressionByAppr2 input (newExprToPush :: restExps) (restOps) (openedBracketsCount) refVariables lastToken variablesComputationPath
                    | _ ->
                        //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                        (ExpressionParsingFailure (NotEnoughOperands remaining), input, openedBracketsCount, refVariables)
        | Some (StableToken.Operator) ->
            (ExpressionParsingFailure (SequencingOfOperatorsNotAllowed input), input, openedBracketsCount, refVariables)
    | Failure (_) ->
        //Failure can also possibly mean that the string is run out of characters to parse but the stacks of operators and expressions need to be resolved
        if openedBracketsCount > 0 then
            (ExpressionParsingFailure (InsufficientParanthesis input), input, openedBracketsCount, refVariables)//Possibly throw an error here
        else
            match input.Trim().Length with
            | 0 ->
                match stackOp with
                | stackTopOp :: restOps ->
                    match stackExp with
                    | topMostExp :: secondTopExp :: restExps ->
                        let newExprToPush = BinaryExpression (secondTopExp, stackTopOp, topMostExp)
                        tryParseMathExpressionByAppr2 input (newExprToPush :: restExps) (restOps) (openedBracketsCount) refVariables lastToken variablesComputationPath
                    | _ ->
                        //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                        (ExpressionParsingFailure (NotEnoughOperands input), input, openedBracketsCount, refVariables)
                | [] ->
                    match stackExp with
                    | onlyStackExp :: [] ->
                        (ExpressionParsingSuccess (Some onlyStackExp),input, openedBracketsCount, refVariables)
                    | _ ->
                        //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                        (ExpressionParsingFailure (TooManyOperands input), input, openedBracketsCount, refVariables)
            | _ ->
                (ExpressionParsingFailure (UnrecognizedInput input), input, openedBracketsCount, refVariables)

type ExpressionEvaluationError =
    | UnexpectedToken of string
    | InvalidOperatorUse of string
    | UnRecognizedToken of string
    | DivideByZeroAttempted of string
    | UnBalancedParanthesis of string
    | ParsingError of MathExpressionParsingFailureType

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
    | Pow ->
        EvaluationSuccess (operand1 ** operand2)
    | Modulo ->
        EvaluationSuccess (operand1 % operand2)
    | Divide ->
        if operand2 = 0. then
            EvaluationFailure (DivideByZeroAttempted ("Expression tried to evaluate : " + operand1.ToString() + operator.ToString() + operand2.ToString()))
        else
            EvaluationSuccess (operand1 / operand2)

let computeUnaryExpression operator operand1 =
    match operator with
    | Exp ->
        EvaluationSuccess (Math.Pow(Math.E, operand1))
    | Sin ->
        EvaluationSuccess (Math.Sin(operand1))
    | Cos ->
        EvaluationSuccess (Math.Cos(operand1))
    | Tan ->
        EvaluationSuccess (Math.Tan(operand1))
    | Sinh ->
        EvaluationSuccess (Math.Sinh(operand1))
    | Cosh ->
        EvaluationSuccess (Math.Cosh(operand1))
    | Tanh ->
        EvaluationSuccess (Math.Tanh(operand1))
    | ASin ->
        EvaluationSuccess (1./Math.Sin(operand1))
    | ACos ->
        EvaluationSuccess (1./Math.Cos(operand1))
    | ATan ->
        EvaluationSuccess (1./Math.Tan(operand1))
    | ASinh ->
        EvaluationSuccess (1./Math.Sinh(operand1))
    | ACosh ->
        EvaluationSuccess (1./Math.Cosh(operand1))
    | ATanh ->
        EvaluationSuccess (1./Math.Tanh(operand1))
    | Log ->
        EvaluationSuccess (Math.Log10(operand1))
    | Ln ->
        EvaluationSuccess (Math.Log(operand1))
    | Floor ->
        EvaluationSuccess (Math.Floor(operand1))
    | Ceil ->
        EvaluationSuccess (Math.Ceiling(operand1))
    | Sqrt ->
        EvaluationSuccess (Math.Sqrt(operand1))
    | Abs ->
        EvaluationSuccess (Math.Abs(operand1))

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
        | UnaryExpression (unaryOp, exp) ->
            let expResult = EvaluateExpression (Some exp)
            match expResult with
            | EvaluationSuccess expDbl ->
                computeUnaryExpression unaryOp expDbl
            | EvaluationFailure failure ->
                EvaluationFailure failure
    | None ->
        EvaluationFailure (UnRecognizedToken "Null expression - Not expected")

let parseAndEvaluateExpression (expressionString) (variablesDict:IDictionary<string,string>) (variableNameBeingEvaluated:string) =
    variables <- variablesDict
    //let parsedExpression = tryParseMathExpression expressionString (None) (None) (None) (false) []
    let parsedExpression = tryParseMathExpressionByAppr2 expressionString [] [] (0) [] None [variableNameBeingEvaluated]
    parsedExpression
    |> fun(expResult, remainingString, _, variablesRef) ->
        match expResult with
        | ExpressionParsingSuccess exp ->
            ((EvaluateExpression exp), remainingString, Seq.ofList variablesRef)
        | ExpressionParsingFailure failure ->
            (EvaluationFailure (ParsingError failure) , remainingString, Seq.ofList variablesRef)

let examplesForMathematicalExpressionParser =

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
            //End region - Testcases from https://lukaszwrobel.pl/blog/math-parser-part-4-tests/

    let errorCases = 
        Map.empty
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

    let variableTestCases =
        ["variableA + variableB"; "variableC + variableA"]

    let developerDeliberatedNotHandledCases =
        [
        "2 + 2(4)"; //Not supported by existing Simpla expression parser
        "(1 + 2)(1 + 4)"; //Not supported by existing Simpla expression parser
        "asin 0"; //Existing Simpla expression parser returns -Infinity
        "2*-2"//Existing simpla expression parser supports this//If this has to be supported, then the expression "2*--2" would also be supported
        ]

    //let listOfExpressions = [exp1;exp2;exp3;exp4;exp5;exp6;exp7;exp8;exp9;exp10;exp11]
    //let listOfExpressions = ["(1 + 2 + 3 * 3 * (1 + 2))"]
    //let listOfExpressions = ["21 + 22 + 23 + 24 + 25 + 26"]
    //let listOfExpressions = [" 5 + + 6"]

    //listOfExpressions |> List.iter printResult
    let mutable countOfFailed = 0
    let mutable countOfSuccess = 0
    let printFailureCase (key:string) (expectedValue: double) =
        let evaluatedResult = parseAndEvaluateExpression key variables "someUniqueName"
        match evaluatedResult with
        | EvaluationFailure someString, _, variablesRef ->
            printfn "\nFailure to evaluate %s" key
            printfn "Evaluation Failure message : %A" someString
            countOfFailed <- countOfFailed + 1
        | EvaluationSuccess evaluatedValue , remaining, variablesRef ->
            if remaining.Trim().Length > 0 then
                printfn "The following expression evaluated successfully but some text remaining"
                printfn "Expression : %A" key
                printfn "Remaining String : %A" remaining
                printfn "Variables referenced : %A" variablesRef

            if evaluatedValue <> expectedValue then
                printfn "The following expression does not evaluate to the expected value :"
                printfn "Expression : %A" key
                printfn "Expected value : %A" expectedValue
                printfn "Obtained value : %A" evaluatedValue
                countOfFailed <- countOfFailed + 1

    let printAllCases (key:string) (expectedValue: double) =
        let evaluatedResult = parseAndEvaluateExpression key variables "someUniqueName"
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
                    

    let printResult (key:string) =
        let evaluatedResult = parseAndEvaluateExpression key variables "someUniqueName"
        match evaluatedResult with
        | EvaluationFailure someString, _, variablesRef ->
            printfn "\nFailure to evaluate %s" key
            printfn "Evaluation Failure message : %A" someString
        | EvaluationSuccess evaluatedValue , remaining, variablesRef ->
            if remaining.Trim().Length > 0 then
                printfn "The following expression evaluated successfully but some text remaining"
                printfn "Expression : %A" key
                printfn "Remaining String : %A" remaining
                printfn "Variables referenced : %A" variablesRef
            else
                printfn "Expression : %A" key
                printfn "Obtained value : %A" evaluatedValue
                printfn "Variables referenced : %A" variablesRef


    printfn "Success Test Cases check : "
    successTestCases |> Map.iter printAllCases
    printfn "\n\nNumber of failed cases : %A " countOfFailed
    printfn "Number of success cases : %A " countOfSuccess

    printfn "Failure test cases error messages check :"
    errorCases |> Map.toSeq |> Seq.map fst |> Seq.iter printResult

    printfn "\n\nVariables test cases : "
    variableTestCases |> List.iter printResult

    printfn "\n\nDeveloper deliberately didn't handle this, cases :"
    developerDeliberatedNotHandledCases |> List.iter printResult
    
    ["\"abc\"";"\"ab\\\"c\"";"\"abc,d";"\"abc\\\"\"\""]
    |> List.iter (fun a -> printfn "%A" (run (parseQuotedString) a))