module BasicParsersForMathExpParsing

open System
open System.Collections.Generic
open FParsec
open MathematicalExpressionParser

let parseCloseBracket = pchar ')' |>> fun(_) -> Token.Bracket BracketClose
let parseOpenBracket = pchar '(' |>> fun(_) -> Token.Bracket BracketOpen

//The below Parsers would be set in the future after they are defined, but the other parsers that are defined before them need them
    //Nested grammers need to make use of this feature
let globalTermParser, globalTermPerserRef = createParserForwardedToRef<ExpressionEvaluationReturnType,UserState>()
let globalExpParser, globalExpParserRef = createParserForwardedToRef<ExpressionEvaluationReturnType, UserState>()

let parseQuotedStringInnerValuesChoices: Parser<_> =
    let alphaNumeric =
        ['a'..'z'] @ ['A'..'Z'] @ ['0'..'9']
        |> List.map (fun a -> a.ToString())
    alphaNumeric @ ["\\\""] @ [" "] @ ["/";":";".";"_";"-"]
    |> List.map (fun s -> pstring s)
    |> choice

let parseQuotedString = 
    ((pchar '"') >>. (many1 parseQuotedStringInnerValuesChoices) .>> (pchar '"'))
    |>> List.reduce (+)
    |>> fun a -> ExpressionOutput (StringExpression a)
    <?> "quoted string"

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

let parsePrefixedUnaryOpTerm =
    let parseUnaryOp = 
        unaryOps
        |> Seq.sortByDescending (fun x -> x.Length)
        |> Seq.map (fun x -> pstring x)
        |> List.ofSeq
        |> choice
        |>> unaryStrToUnaryOpUnion
    let expParsed = (((parseUnaryOp .>> spaces)) .>>. globalTermParser)
    expParsed
    |>> (fun (unaryOp, (ExpressionOutput (expr))) -> ExpressionOutput ((UnaryExpression (unaryOp, expr))) )

let parseMasterVariable: Parser<_> =
    let variables = List.ofSeq (masterVariables.Keys)
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