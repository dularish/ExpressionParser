module BasicParsersForMathExpParsing

open System
open System.Collections.Generic
open ParserBuildingBlocks
open MathematicalExpressionParser
open BasicParsers
open LazyParserBlocks

let parseCloseBracket = pchar ')' |>> fun(_) -> Token.Bracket BracketClose
let parseOpenBracket = pchar '(' |>> fun(_) -> Token.Bracket BracketOpen

let parseQuotedStringInnerValuesChoices =
    let alphabets =
        ['a'..'z'] @ ['A'..'Z']
        |> List.map (fun a -> a.ToString())
    alphabets @ ["\\\""] @ [" "]
    |> List.map (fun s -> pstring s)
    |> choice

let parseQuotedString = 
    ((pchar '"') >>. (many parseQuotedStringInnerValuesChoices) .>> (pchar '"'))
    |>> List.reduce (+)
    |>> fun a -> ExpressionWithVariables (StringExpression a, [])
    <?> "quoted string"

let parseDoubleNum =
    (opt (pchar '-')) .>>. (many1 parseDigit) .>>. (opt ((pchar '.') .>>. (many1 parseDigit) ))
    |>> (fun (wholeNums, decimalPart) ->
        let wholeNumsWithNegSignIfNeeded =
            match wholeNums with
            | Some _ , wholeNumsList ->
                '-' :: wholeNumsList
            | _ , wholeNumsList ->
                wholeNumsList
        match decimalPart with
        | Some (decimalPoint, decimalPartDigits) ->
            let doubleExp = System.String(List.toArray(wholeNumsWithNegSignIfNeeded @ [decimalPoint] @ decimalPartDigits))
                            |> double
            doubleExp
        | None ->
            let doubleExp = System.String(List.toArray(wholeNumsWithNegSignIfNeeded)) |> double
            doubleExp)

let parseNumericTerm =
    parseDoubleNum
    |>> (fun doubleNum -> ExpressionWithVariables (Expression.Constant doubleNum, []))
    <?> "numeric term"

let parseNumArray =
    pchar '[' .>> spaces >>. parseDoubleNum .>>. (many (spaces >>. pchar ',' >>. spaces >>. parseDoubleNum)) .>> pchar ']'
    |>> (fun (head, tail) -> ExpressionWithVariables (Expression.NumArray (head::tail), []))
    <?> "num array"

let parseBoolStringAsDouble =
    ((pstring "true") <|> (pstring "false"))
    |>> (fun s -> 
                let doubleNum = if s = "true" then 1. else 0.
                ExpressionWithVariables (Expression.Constant doubleNum, []))

let parseBracketedExpression (expParser:(string list -> unit -> Parser<ExpressionEvaluationReturnType>)) (variablesRef) = fun() ->
    (between parseOpenBracket (spaces >>. (expParser variablesRef)() .>> spaces) (parseCloseBracket <?> "matching closing paranthesis"))

let parsePrefixedUnaryOpTerm (termParser:(unit -> Parser<ExpressionEvaluationReturnType>))= fun() ->
    let parseUnaryOp = 
        unaryOps
        |> Seq.sortByDescending (fun x -> x.Length)
        |> Seq.map (fun x -> pstring x)
        |> List.ofSeq
        |> choice
        |>> unaryStrToUnaryOpUnion
    let expParsed = ((fun () -> (parseUnaryOp .>> spaces)) .^>>^. termParser)
    expParsed
    |>> (fun (unaryOp, (ExpressionWithVariables (expr, varList))) -> ExpressionWithVariables ((UnaryExpression (unaryOp, expr)), varList) )