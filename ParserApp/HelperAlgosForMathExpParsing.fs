module HelperAlgosForMathExpParsing
open MathematicalExpressionParser

type ShuntingYardStreamCandidateTypes =
    | MaybeOperator of Token option
    | Expr of Expression

//This implementation is based on shunting yard algorithm
//https://brilliant.org/wiki/shunting-yard-algorithm/

let rec performShuntingYardLogicOnList expStack opStack streamList =
    match streamList with
    | [] ->
        match opStack with
        | stackTopOp :: restOps ->
            match expStack with
            | topMostExp :: secondTopExp :: restExps ->
                let newExprToPush = BinaryExpression (secondTopExp, stackTopOp, topMostExp)
                performShuntingYardLogicOnList (newExprToPush :: restExps) (restOps) []
            | _ ->
                //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                Expression.Constant -1.
        | [] ->
            match expStack with
            | onlyStackExp :: [] ->
                onlyStackExp
            | _ ->
                //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                Expression.Constant -1.
    | topMostOfStream :: restStream ->
        match topMostOfStream with
        | MaybeOperator operator ->
            match operator with
            | None ->
                //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                Expression.Constant -1.
            | Some ( BinaryOperator someOp) ->
                match opStack with
                | [] ->
                    performShuntingYardLogicOnList expStack (someOp :: opStack) restStream
                | opStackTop :: opStackRest ->
                    if ((getPriority(someOp) > getPriority(opStackTop)) || (((someOp = opStackTop) && ((getAssociativity someOp) = Right )))) then
                        performShuntingYardLogicOnList expStack (someOp :: opStack) restStream
                    else
                        match expStack with
                        | topMostExp :: secondTopExp :: restExps ->
                            let newExprToPush = BinaryExpression (secondTopExp, opStackTop, topMostExp)
                            performShuntingYardLogicOnList (newExprToPush :: restExps) (opStackRest) streamList
                        | _ ->
                            //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                            Expression.Constant -1.
            | _ ->
                //Control to this point not expected, would mean the logical problem unhandled at some other point, which the developer has to be know
                Expression.Constant -1.
        | Expr someExp ->
            performShuntingYardLogicOnList (someExp :: expStack) opStack restStream

let convertContinuousTermsToSingleExpression (firstExp:(ExpressionEvaluationReturnType),(operatorExpPairList:(Token option * (ExpressionEvaluationReturnType))list)) =
    let secondArgumentConvertedToSingleExprList =
        match operatorExpPairList with
        | [] -> []
        | _ ->
            operatorExpPairList
            |> List.map (fun (x,(ExpressionWithVariables (y,varList))) -> 
                            match x with
                            | Some operatorToken ->
                                [MaybeOperator x]@[Expr y]
                            | None ->
                                //When there are two valid terms not separated by an operator, implicitly assumed to be of multiplication
                                //Example cases : "2(4)", "(1+2)(2+3)"
                                [MaybeOperator (Some (BinaryOperator Multiply))]@[Expr y])
            |> List.reduce (@)
    let secondArgumentConvertedToSingleVariablesList =
        match operatorExpPairList with
        | [] -> []
        | _ ->
            operatorExpPairList
            |> List.map (fun (x,(ExpressionWithVariables (y,varList))) -> varList)
            |> List.reduce (@)
    match firstExp with
    | ExpressionWithVariables (firstExpr, firstVarList) ->
        let expr = ((Expr ((firstExpr))) :: secondArgumentConvertedToSingleExprList)
                    |> performShuntingYardLogicOnList [] []
        let varList = (firstVarList) @ (secondArgumentConvertedToSingleVariablesList)
                      |> List.distinct
        ExpressionWithVariables (expr, varList)