module MathematicalExpressionParser
open System
open FParsec
open System.Collections.Generic

type UserState = unit
type Parser<'a> = Parser<'a, UserState>

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
    | EqualTo
    | NotEqualTo
    | GreaterThanOrEqualTo
    | LessThanOrEqualTo
    | GreaterThan
    | LessThan
    | LogicalAnd
    | LogicalOr

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
    | Not
    | Mean
    | Min
    | Max
    | Sd
    | Sum
    | SumSquared

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
    | StringExpression of string
    | NumArray of (double list)

type ExpressionEvaluationReturnType =
    |ExpressionWithVariables of (Expression * string list)

type Token = 
    | DoubleConstant of double
    | BinaryOperator of BinaryOperator
    | UnaryOperator of UnaryOperator
    | Bracket of Brackets
    | Variable of string
    | MasterKeywordVariable of string
    | QuotedString of string

type StableToken =
    | Operator of BinaryOperator
    | Expression of Expression

let binaryOps = 
        ["+";"-";"/";"*";"^";"%";"==";"!=";">=";"<=";">";"<";"&&";"||"]

let unaryOps =
    ["exp";"sin";"cos";"tan";"acos";"asin";"atan";"sinh";"cosh";"tanh";"asinh";"acosh";"atanh";"log";"ln";"floor";"ceil";"sqrt";"abs";"!";"mean";"min";"max";"sd";"sum";"sumsquared"]

let arithmeticCharToUnion input =
    if input = "+" then (BinaryOperator (Plus))
    elif input = "-" then (BinaryOperator (Minus))
    elif input = "*" then (BinaryOperator (Multiply))
    elif input = "^" then (BinaryOperator (Pow))
    elif input = "%" then (BinaryOperator (Modulo))
    elif input = "/" then (BinaryOperator (Divide))
    elif input = "==" then (BinaryOperator (EqualTo))
    elif input = "!=" then (BinaryOperator (NotEqualTo))
    elif input = ">=" then (BinaryOperator (GreaterThanOrEqualTo))
    elif input = "<=" then (BinaryOperator (LessThanOrEqualTo))
    elif input = ">" then (BinaryOperator (GreaterThan))
    elif input = "<" then (BinaryOperator (LessThan))
    elif input = "&&" then (BinaryOperator (LogicalAnd))
    else (BinaryOperator (LogicalOr))

let unaryStrToUnaryOpUnion input =
    if input = "exp" then ( (Exp))
    elif input = "sin" then ( (Sin))
    elif input = "cos" then ( (Cos))
    elif input = "tan" then ((Tan))
    elif input = "asin" then ((ASin))
    elif input = "acos" then ((ACos))
    elif input = "atan" then ((ATan))
    elif input = "sinh" then ((Sinh))
    elif input = "cosh" then ((Cosh))
    elif input = "tanh" then ((Tanh))
    elif input = "asinh" then ((ASinh))
    elif input = "acosh" then ((ACosh))
    elif input = "atanh" then ((ATanh))
    elif input = "log" then ((Log))
    elif input = "ln" then ((Ln))
    elif input = "floor" then ((Floor))
    elif input = "ceil" then ((Ceil))
    elif input = "sqrt" then ((Sqrt))
    elif input = "abs" then ((Abs))
    elif input = "sum" then ((Sum))
    elif input = "sumsquared" then ((SumSquared))
    elif input = "mean" then ((Mean))
    elif input = "min" then ((Min))
    elif input = "max" then ((Max))
    elif input = "sd" then ((Sd))
    else ((Not))

let parseArithmeticOp: Parser<_> =
    binaryOps
    |> List.map (fun x -> pstring x)
    |> choice
    |>> arithmeticCharToUnion
    <?> "arithmetic operator"

let masterVariableNameToToken inputString =
    MasterKeywordVariable (inputString)

let parseMasterVariable: Parser<_> =
    masterVariables.Keys
    |> Seq.sortByDescending (fun x -> x.Length)
    |> Seq.map (fun x -> pstring x)
    |> List.ofSeq
    |> choice
    |>> masterVariableNameToToken
    <?> "master variable"

type EntryType = 
    | Token of Token
    | TokenList of EntryType list

let getPriority operator =
    match operator with
    | LogicalAnd -> 1
    | LogicalOr -> 1
    | EqualTo -> 2
    | NotEqualTo -> 2
    | GreaterThan -> 2
    | GreaterThanOrEqualTo -> 2
    | LessThan -> 2
    | LessThanOrEqualTo -> 2
    | Plus -> 3
    | Minus -> 3
    | Multiply -> 4
    | Divide -> 4
    | Modulo -> 4
    | Pow -> 5

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
    | IncompleteParsing of string

type MathExpressionParserResult =
    | ExpressionParsingSuccess of Expression option
    | ExpressionParsingFailure of MathExpressionParsingFailureType

type ExpressionEvaluationError =
    | UnexpectedToken of string
    | InvalidOperatorUse of string
    | UnRecognizedToken of string
    | DivideByZeroAttempted of string
    | UnBalancedParanthesis of string
    | ParsingError of MathExpressionParsingFailureType
    | ArrayTypeNotSupportedAsReturnType of string

type AllowedEvaluationResultTypes =
    | Double of double
    | String of string
    | NumericArray of (double list)

let getStringValueOfEvaluationResultType (evaluationResultTypeValue) =
    match evaluationResultTypeValue with
    | Double x -> x.ToString()
    | String x -> x
    | NumericArray x -> x.ToString()

type ExpressionEvaluationResult =
    | EvaluationSuccess of AllowedEvaluationResultTypes
    | EvaluationFailure of ExpressionEvaluationError

let doubleToBool x =
    x = 1.

let boolToDouble x =
    if x = true then (1.) else (0.)

let computeBinaryExpression (operand1:AllowedEvaluationResultTypes) operator (operand2:AllowedEvaluationResultTypes) =
    match (operand1, operand2) with
    | (Double operand1DoubleValue, Double operand2DoubleValue) ->
        match operator with
        | Plus ->
            EvaluationSuccess (Double (operand1DoubleValue + operand2DoubleValue))
        | Minus ->
            EvaluationSuccess (Double (operand1DoubleValue - operand2DoubleValue))
        | Multiply ->
            EvaluationSuccess (Double (operand1DoubleValue * operand2DoubleValue))
        | Pow ->
            EvaluationSuccess (Double (operand1DoubleValue ** operand2DoubleValue))
        | Modulo ->
            EvaluationSuccess (Double (operand1DoubleValue % operand2DoubleValue))
        | Divide ->
            if operand2DoubleValue = 0. then
                EvaluationFailure (DivideByZeroAttempted ("Expression tried to evaluate : " + operand1DoubleValue.ToString() + operator.ToString() + operand2DoubleValue.ToString()))
            else
                EvaluationSuccess (Double (operand1DoubleValue / operand2DoubleValue))
        | LogicalAnd ->
            let operand1BoolValue = doubleToBool operand1DoubleValue
            let operand2BoolValue = doubleToBool operand2DoubleValue
            EvaluationSuccess (Double (boolToDouble (operand1BoolValue && operand2BoolValue)))
        | LogicalOr ->
            let operand1BoolValue = doubleToBool operand1DoubleValue
            let operand2BoolValue = doubleToBool operand2DoubleValue
            EvaluationSuccess (Double (boolToDouble (operand1BoolValue || operand2BoolValue)))
        |LessThan ->
            EvaluationSuccess (Double (boolToDouble (operand1DoubleValue < operand2DoubleValue)))
        | LessThanOrEqualTo ->
            EvaluationSuccess (Double (boolToDouble (operand1DoubleValue <= operand2DoubleValue)))
        | GreaterThan ->
            EvaluationSuccess (Double (boolToDouble (operand1DoubleValue > operand2DoubleValue)))
        | GreaterThanOrEqualTo ->
            EvaluationSuccess (Double (boolToDouble (operand1DoubleValue >= operand2DoubleValue)))
        | EqualTo ->
            EvaluationSuccess (Double (boolToDouble (operand1DoubleValue = operand2DoubleValue)))
        | NotEqualTo ->
            EvaluationSuccess (Double (boolToDouble (operand1DoubleValue <> operand2DoubleValue)))
    | (Double _, String _)
    | (String _, Double _)
    | (String _, String _) ->
        match operator with
        | Plus ->
            EvaluationSuccess (String (getStringValueOfEvaluationResultType(operand1) + getStringValueOfEvaluationResultType(operand2)))
        | _ ->
           EvaluationFailure (InvalidOperatorUse (sprintf "Operator %A cannot be used with String types" operator)) 
    | (NumericArray _ , _)
    | (_, NumericArray _)
    | (NumericArray _, NumericArray _) ->
        EvaluationFailure (InvalidOperatorUse (sprintf "Numeric array types cannot be used with binary operator"))

let computeUnaryExpression operator (operand1:AllowedEvaluationResultTypes) =
    match operand1 with
    | (String _) ->
        EvaluationFailure (InvalidOperatorUse (sprintf "Unary Operator %A cannot be used with String types" operator))
    | (Double operand1DoubleValue) ->
        match operator with
        | Exp ->
            EvaluationSuccess (Double (Math.Pow(Math.E, operand1DoubleValue)))
        | Sin ->
            EvaluationSuccess (Double (Math.Sin(operand1DoubleValue)))
        | Cos ->
            EvaluationSuccess (Double (Math.Cos(operand1DoubleValue)))
        | Tan ->
            EvaluationSuccess (Double (Math.Tan(operand1DoubleValue)))
        | Sinh ->
            EvaluationSuccess (Double (Math.Sinh(operand1DoubleValue)))
        | Cosh ->
            EvaluationSuccess (Double (Math.Cosh(operand1DoubleValue)))
        | Tanh ->
            EvaluationSuccess (Double (Math.Tanh(operand1DoubleValue)))
        | ASin ->
            EvaluationSuccess (Double (1./Math.Sin(operand1DoubleValue)))
        | ACos ->
            EvaluationSuccess (Double (1./Math.Cos(operand1DoubleValue)))
        | ATan ->
            EvaluationSuccess (Double (1./Math.Tan(operand1DoubleValue)))
        | ASinh ->
            EvaluationSuccess (Double (1./Math.Sinh(operand1DoubleValue)))
        | ACosh ->
            EvaluationSuccess (Double (1./Math.Cosh(operand1DoubleValue)))
        | ATanh ->
            EvaluationSuccess (Double (1./Math.Tanh(operand1DoubleValue)))
        | Log ->
            EvaluationSuccess (Double (Math.Log10(operand1DoubleValue)))
        | Ln ->
            EvaluationSuccess (Double (Math.Log(operand1DoubleValue)))
        | Floor ->
            EvaluationSuccess (Double (Math.Floor(operand1DoubleValue)))
        | Ceil ->
            EvaluationSuccess (Double (Math.Ceiling(operand1DoubleValue)))
        | Sqrt ->
            EvaluationSuccess (Double (Math.Sqrt(operand1DoubleValue)))
        | Abs ->
            EvaluationSuccess (Double (Math.Abs(operand1DoubleValue)))
        | Not ->
            EvaluationSuccess (Double (boolToDouble(not (doubleToBool (operand1DoubleValue)))))
        | x ->
            EvaluationFailure (InvalidOperatorUse (sprintf "Unary Operator %A cannot be used with Double types" x))
    | (NumericArray numericArray) ->
        match operator with
        | Mean ->
            EvaluationSuccess (Double (List.average numericArray))
        | Min ->
            EvaluationSuccess (Double (List.min numericArray))
        | Max ->
            EvaluationSuccess (Double (List.max numericArray))
        | Sum ->
            EvaluationSuccess (Double (List.sum numericArray))
        | SumSquared ->
            EvaluationSuccess (Double (numericArray |> List.map (fun x -> x * x) |> List.sum))
        | Sd ->
            let n = double numericArray.Length
            let mean = numericArray |> List.average
            let sumOfSquaresOfDiff = numericArray |> List.map (fun x -> Math.Pow(x - mean,2.)) |> List.sum
            let variance = sumOfSquaresOfDiff / n
            let sd = Math.Sqrt variance
            EvaluationSuccess(Double (sd))
        | x ->
            EvaluationFailure (InvalidOperatorUse (sprintf "Unary Operator %A cannot be used with Array types" x))

let rec EvaluateExpression (exp: Expression option) =
    match exp with
    | Some someExp ->
        match someExp with
        | Constant doubleConstant ->
            EvaluationSuccess (Double doubleConstant)
        | StringExpression quotedString ->
            EvaluationSuccess (String quotedString)
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
        | NumArray x ->
            EvaluationSuccess (NumericArray x)
    | None ->
        EvaluationFailure (UnRecognizedToken "Null expression - Not expected")