﻿module BasicParsersForMathExpParsing

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
    let alphabets =
        ['a'..'z'] @ ['A'..'Z']
        |> List.map (fun a -> a.ToString())
    alphabets @ ["\\\""] @ [" "]
    |> List.map (fun s -> pstring s)
    |> choice

let parseQuotedString = 
    ((pchar '"') >>. (many parseQuotedStringInnerValuesChoices) .>> (pchar '"'))
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