module BasicParsers

open System
open System.Collections.Generic
open FParsec
open ExpParserConfigurables

let parseCloseBracket = pchar ')' |>> fun(_) -> Token.Bracket BracketClose
let parseOpenBracket = pchar '(' |>> fun(_) -> Token.Bracket BracketOpen

let parseComma = pchar ',' <?> "Comma"

//The below Parsers would be set in the future after they are defined, but the other parsers that are defined before them need them
    //Nested grammers need to make use of this feature
let globalTermParser, globalTermPerserRef = createParserForwardedToRef<ExpressionEvaluationReturnType,UserState>()
let globalExpParser, globalExpParserRef = createParserForwardedToRef<ExpressionEvaluationReturnType, UserState>()

let parseQuotedString: Parser<_> =
    let normalChar = 
        satisfy (fun s -> s <> '\\' && s <> '"')

    let unescapeChar s = match s with
                         | 'n' -> '\n'
                         | 'r' -> '\r'
                         | 't' -> '\t'
                         | s -> s

    let escapedChar = 
        pchar '\\' >>. ((anyOf "\\nrt\"" |>> unescapeChar) <?> "valid escaped character")
        <?> "escape sequence"

    ((pchar '"') >>. ((many1Chars (normalChar <|> escapedChar)) <?> "atleast one character") .>> (pchar '"'))
    |>> fun a -> ExpressionOutput (StringExpression a)
    <?> "quoted string"

let parseSimplaString: Parser<_> =
    let normalChar = 
        satisfy (fun s -> s <> '"' && s <> '\\')
        |>> fun a -> a

    let escapedChar = 
        pchar '\\' .>>. (opt (anyOf "\"\\"))
        |>> (fun (a,b) -> 
                match b with
                | Some someB ->
                    someB.ToString()
                | _ -> a.ToString())
        <?> "escape sequence"

    ((pchar '"') >>. ((manyStrings ((many1Chars normalChar) <|> (escapedChar)))) .>> (pchar '"'))
    |>> fun a -> ExpressionOutput (StringExpression a)
    <?> "SIMPLA string"

let parseNumericTerm: Parser<_> =
    pfloat
    |>> (fun doubleNum -> ExpressionOutput (Expression.Constant doubleNum))
    <?> "numeric term"

let parseNumArray: Parser<_> =
    pchar '[' .>> spaces >>. pfloat .>>. (many (spaces >>. pchar ',' >>. spaces >>. pfloat)) .>> pchar ']'
    |>> (fun (head, tail) -> ExpressionOutput (Expression.NumArray (head::tail)))
    <?> "num array"

let parseBoolStringAsDouble: Parser<_> =
    ((pstring "true") <|> (pstring "false"))
    |>> (fun s -> 
                let doubleNum = if s = "true" then 1. else 0.
                ExpressionOutput (Expression.Constant doubleNum))

let parseBracketedExpression =
    (between parseOpenBracket (parseCloseBracket <?> "matching closing paranthesis") (spaces >>. (globalExpParser) .>> spaces))

let parseSign: Parser<_> =
    ((pchar '+') <|> (pchar '-'))
    |>> (fun s -> 
            match s with
            | '-' -> MinusSign
            | '+' -> PlusSign)

let parseSignedParser (pExp:(Parser<ExpressionEvaluationReturnType>)) =
    ((opt parseSign) .>>.? (spaces >>? pExp))
    |>> (fun (sign,exp) ->
            match sign with
            | Some someSign ->
                match someSign with
                | PlusSign -> exp
                | MinusSign -> 
                    match exp with
                    | ExpressionOutput someExp -> ExpressionOutput (BinaryExpression(Expression.Constant(-1.), BinaryOperator.Multiply, someExp))
            | None -> exp)


let parseSignedBracketedExpression =
    parseSignedParser parseBracketedExpression

let parsePrefixedUnaryOpTerm =
    let parseUnaryOp = 
        unaryOps
        |> Seq.sortByDescending (fun x -> x.Key.Length)
        |> Seq.map (fun x -> pstring x.Key)
        |> List.ofSeq
        |> choice
        |>> (fun s -> unaryOps.[s])
    let expParsed = (((parseUnaryOp .>> spaces)) .>>. globalTermParser)
    expParsed
    |>> (fun (unaryOp, (ExpressionOutput (expr))) -> ExpressionOutput ((UnaryExpression (unaryOp, expr))) )

let parseSignedUnaryOpTerm =
    parseSignedParser parsePrefixedUnaryOpTerm

let parseBinaryFuncTerm =
    let parseBinaryFunc =
        binaryFunctions
        |> Seq.sortByDescending(fun x -> x.Key.Length)
        |> Seq.map (fun x -> pstring x.Key)
        |> List.ofSeq
        |> choice
        |>> (fun s -> binaryFunctions.[s])
    let parseTupleTerms = parseOpenBracket >>. spaces >>. globalTermParser .>> spaces .>> parseComma .>> spaces .>>. globalTermParser .>> spaces .>> parseCloseBracket
    let expParsed = ((parseBinaryFunc .>> spaces) .>>. parseTupleTerms)
    expParsed
    |>> (fun (binaryFunc, ((ExpressionOutput expr1), (ExpressionOutput expr2))) -> ExpressionOutput (BinaryFuncExpression (binaryFunc,expr1,expr2)))

let parseSignedBinaryFuncTerm =
    parseSignedParser parseBinaryFuncTerm

let parseMasterVariable: Parser<_> =
    let variables = 
        List.ofSeq (masterVariables.Keys)
        |> List.sortByDescending (fun s -> s.Length)
    let pChoiceVars = 
        variables
        |> List.map (fun var -> pstring var)
        |> choice
    pChoiceVars
    >>= (fun masterVar -> 
            runParserOnString ((globalExpParser)) UserState.Default masterVar masterVariables.[masterVar]
            |> (fun x ->
                    match x with
                    | Success ((ExpressionOutput (expressionParsed)), _, _) ->
                        preturn (ExpressionOutput(expressionParsed))
                    | Failure (label, err, pos) ->
                        fail (sprintf "Error in evaluating the expression for the master variable %s" masterVar)
            ))

    <?> "master variable"

let parseSignedMasterVariable =
    parseSignedParser parseMasterVariable