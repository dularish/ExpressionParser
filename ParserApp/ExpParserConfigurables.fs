module ExpParserConfigurables
open System
open FParsec
open System.Collections.Generic

type UserState = 
    {
        VariablesReferenced:string list;
        VariablesReferencedByThisLayer:string list;
        VariablesDict:IDictionary<string,string>;
        VariableName:string
    }
    with
    static member Default = {VariablesReferenced = []; VariablesDict=dict[]; VariableName="unNamedVar"; VariablesReferencedByThisLayer = []}

let softPushRefVarForThisLayer varName= 
    updateUserState (fun s -> 
        if (s.VariablesReferencedByThisLayer |> List.contains varName) then
            s
        else
            {s with VariablesReferencedByThisLayer = varName::s.VariablesReferencedByThisLayer}
        )

let setVariableNameToUserState varName =
    updateUserState (fun s ->
        {s with VariableName = varName})

let initializeVariablesDict variablesDict =
    updateUserState (fun s ->
        {s with VariablesDict = variablesDict})

let initializeRefVars variablesRef =
    updateUserState (fun s ->
        {s with VariablesReferenced = variablesRef})

let checkVarNotBeingSelfRef varName =
    userStateSatisfies (fun s -> 
                            not (s.VariableName = varName))
    <?> (sprintf "Self referencing of variable %s" varName)

let checkVarNotRefPrior varName =
    userStateSatisfies (fun s -> 
                            not (s.VariablesReferenced |> List.contains varName))
    <?> (sprintf "Circular referencing of variable %s" varName)

let parseVariableFromUserState =
    getUserState
    >>= (fun s ->
            let variables = List.ofSeq (s.VariablesDict.Keys)
            let pChoiceVars = variables
                                |> List.sortByDescending (fun s -> s.Length)
                                |> List.map (fun var -> pstring var)
                                |> choice
            let pvalidVar = pChoiceVars
                            >>= (fun s ->
                                    (checkVarNotBeingSelfRef s) >>. (checkVarNotRefPrior s) >>. (preturn s))
            pvalidVar
            >>= (fun validVar -> preturn (validVar, s.VariablesDict.[validVar], s.VariablesDict, s.VariablesReferenced, s.VariableName))
            )
    <?> "Parsing variable failed"

type Parser<'a> = Parser<'a, UserState>

let masterVariables = dict [
                "pi", Math.PI.ToString();
                "e", Math.E.ToString();
                "Math.PI", Math.PI.ToString()]

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

type BinaryFunction =
    |ATan2
    |PowFn

type Operator =
    | BinaryOperator of BinaryOperator
    | UnaryOperator of UnaryOperator

type Brackets =
    | BracketOpen
    | BracketClose

type Expression = 
    | Constant of double
    | BinaryExpression of Expression*BinaryOperator*Expression
    | TernaryExpression of Expression * Expression * Expression
    | UnaryExpression of UnaryOperator * Expression
    | BinaryFuncExpression of BinaryFunction * Expression * Expression
    | StringExpression of string
    | NumArray of (double list)

type ExpressionEvaluationReturnType =
    |ExpressionOutput of Expression

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

type Sign =
    |PlusSign
    |MinusSign

let binaryOps = 
        dict["+",(BinaryOperator (Plus));"-",(BinaryOperator (Minus));"/",(BinaryOperator (Divide));"*",(BinaryOperator (Multiply));"^",(BinaryOperator (Pow));"%",(BinaryOperator (Modulo));"==",(BinaryOperator (EqualTo));"!=",(BinaryOperator (NotEqualTo));">=",(BinaryOperator (GreaterThanOrEqualTo));"<=",(BinaryOperator (LessThanOrEqualTo));">",(BinaryOperator (GreaterThan));"<",(BinaryOperator (LessThan));"&&",(BinaryOperator (LogicalAnd));"||",(BinaryOperator (LogicalOr))]

let unaryOps =
    dict["exp",Exp;"sin",Sin;"cos",Cos;"tan",Tan;"acos",ACos;"asin",ASin;"atan",ATan;"sinh",ASinh;"cosh",Cosh;"tanh",Tanh;"asinh",ASinh;"acosh",ACosh;"atanh",ATanh;"log",Log;"ln",Ln;"floor",Floor;"ceil",Ceil;"sqrt",Sqrt;"abs",Abs;"!",Not;"mean",Mean;"min",Min;"max",Max;"sd",Sd;"sum",Sum;"sumsquared",SumSquared]

let binaryFunctions =
    dict["atan2",ATan2;"pow",PowFn]

let parseArithmeticOp: Parser<_> =
    binaryOps
    |> Seq.map (fun x -> pstring x.Key)
    |> choice
    |>> (fun s -> binaryOps.[s])
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
        | EqualTo ->
            EvaluationSuccess (Double (boolToDouble(getStringValueOfEvaluationResultType(operand1) = getStringValueOfEvaluationResultType(operand2))))
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

let computeBinaryFuncExpression (binaryFunc:BinaryFunction) (operand1:AllowedEvaluationResultTypes) (operand2:AllowedEvaluationResultTypes) =
    match operand1, operand2 with
    | (Double operand1DblValue, Double operand2DblValue) ->
        match binaryFunc with
        | ATan2 ->
            EvaluationSuccess (Double (Math.Atan2(operand1DblValue, operand2DblValue)))
        | PowFn ->
            EvaluationSuccess (Double (Math.Pow(operand1DblValue, operand2DblValue)))
        | x ->
            EvaluationFailure (InvalidOperatorUse (sprintf "Binary function operator %A cannot be used with Double types" x))
    | _ ->
        match binaryFunc with
        | x ->
            EvaluationFailure (InvalidOperatorUse (sprintf "Binary function operator %A cannot be used" x))

let computeTernaryExpression (conditionRes:AllowedEvaluationResultTypes) (trueRes:AllowedEvaluationResultTypes) (falseRes:AllowedEvaluationResultTypes) =
    match (conditionRes) with
        | (Double conditionResDblValue) ->
            match (trueRes, falseRes) with
            | (Double trueResDblValue, Double falseResDblValue) ->
                let resultToReturn = if conditionResDblValue = 1. then trueResDblValue else falseResDblValue
                EvaluationSuccess (Double resultToReturn)
            | (String trueResStringValue, String falseResStringValue) ->
                let resultToReturn = if conditionResDblValue = 1. then trueResStringValue else falseResStringValue
                EvaluationSuccess(String resultToReturn)
            | _ ->
                EvaluationFailure (InvalidOperatorUse (sprintf "Ternary operator should contain both the result expressions of same type and should either be String or Double"))
        | _ ->
            EvaluationFailure (InvalidOperatorUse (sprintf "Ternary operation condition expression should be of double"))

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
        | TernaryExpression (condition, trueExp, falseExp) ->
            let conditionResult = EvaluateExpression (Some condition)
            let trueResult = EvaluateExpression (Some trueExp)
            let falseResult = EvaluateExpression (Some falseExp)
            match (conditionResult, trueResult, falseResult) with
            | (EvaluationSuccess conditionRes, EvaluationSuccess trueRes, EvaluationSuccess falseRes) ->
                computeTernaryExpression conditionRes trueRes falseRes
            | (EvaluationFailure failure, _, _) ->
                EvaluationFailure failure
            | (_, EvaluationFailure failure, _) ->
                EvaluationFailure failure
            | (_, _, EvaluationFailure failure) ->
                EvaluationFailure failure
        | UnaryExpression (unaryOp, exp) ->
            let expResult = EvaluateExpression (Some exp)
            match expResult with
            | EvaluationSuccess expDbl ->
                computeUnaryExpression unaryOp expDbl
            | EvaluationFailure failure ->
                EvaluationFailure failure
        | BinaryFuncExpression (binaryFunc,expr1, expr2) ->
            let expr1Result = EvaluateExpression (Some expr1)
            let expr2Result = EvaluateExpression (Some expr2)
            match expr1Result, expr2Result with
            | (EvaluationSuccess expr1Result, EvaluationSuccess expr2Result) ->
                computeBinaryFuncExpression binaryFunc expr1Result expr2Result
            | (EvaluationFailure failure, _) ->
                EvaluationFailure failure
            | (_, EvaluationFailure failure) ->
                EvaluationFailure failure
        | NumArray x ->
            EvaluationSuccess (NumericArray x)
    | None ->
        EvaluationFailure (UnRecognizedToken "Null expression - Not expected")