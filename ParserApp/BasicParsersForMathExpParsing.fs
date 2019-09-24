module BasicParsersForMathExpParsing

open System
open System.Collections.Generic
open ParserBuildingBlocks
open MathematicalExpressionParser
open BasicParsers
open LazyParserBlocks

let parseCloseBracket = pChar ')' |>> fun(_) -> Token.Bracket BracketClose
let parseOpenBracket = pChar '(' |>> fun(_) -> Token.Bracket BracketOpen

let parseQuotedStringInnerValuesChoices =
    let alphabets =
        ['a'..'z'] @ ['A'..'Z']
        |> List.map (fun a -> a.ToString())
    alphabets @ ["\\\""]
    |> List.map (fun s -> pString s)
    |> choice
    <?> "quoted string"

let parseQuotedString = 
    ((pChar '"') >>. (many parseQuotedStringInnerValuesChoices) .>> (pChar '"'))
    |>> List.reduce (+)
    |>> fun a -> QuotedString a

let parseNumericTerm =
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
            let doubleExp = System.String(List.toArray(wholeNumsWithNegSignIfNeeded @ [decimalPoint] @ decimalPartDigits))
                            |> double |> Expression.Constant
            ExpressionWithVariables (doubleExp, [])
        | None ->
            let doubleExp = System.String(List.toArray(wholeNumsWithNegSignIfNeeded)) |> double |> Expression.Constant
            ExpressionWithVariables (doubleExp, []))
    <?> "numeric term"

let parseBracketedExpression (expParser:(string list -> unit -> Parser<ExpressionEvaluationReturnType>)) (variablesRef) = fun() ->
    (between parseOpenBracket (spaces >>. (expParser variablesRef)() .>> spaces) (parseCloseBracket <?> "matching closing paranthesis"))

let parsePrefixedUnaryOpTerm (termParser:(unit -> Parser<ExpressionEvaluationReturnType>))= fun() ->
    let parseUnaryOp = 
        unaryOps
        |> Seq.sortByDescending (fun x -> x.Length)
        |> Seq.map (fun x -> pString x)
        |> List.ofSeq
        |> choice
        |>> unaryStrToUnaryOpUnion
    let expParsed = ((fun () -> (parseUnaryOp .>> spaces)) .^>>^. termParser)
    expParsed
    |>> (fun (unaryOp, (ExpressionWithVariables (expr, varList))) -> ExpressionWithVariables ((UnaryExpression (unaryOp, expr)), varList) )