module BackedUpApproach

open MathematicalExpressionParser
open ParserBuildingBlocks

//This implementation is a Mathematical expression parser which parses by applying BODMAS
//This implementation was developed without considering Shunting yard algorithm, in my opinion lacks readability
let rec tryParseMathExpression input (stackExp:Expression option) (stackOp: BinaryOperator option) (memoryOp: BinaryOperator option) (isOpened:bool) (refVariables:string list)=
    
    //Resuable function to handle the expression that is found, handling for different conditions of stackExp, stackOp, remaining expression
    let handleFoundExpression = fun(expression: Expression, remaining, variablesRefPassed) ->
        match stackExp with
        | None ->
            (tryParseMathExpression remaining (Some expression) (None) (memoryOp) (isOpened) variablesRefPassed)
        | Some someStackExp ->
            let expressionMatchedCaseResult = run (parseAToken()) remaining
            match expressionMatchedCaseResult with
            | Success (Token.BinaryOperator opMatched, remainingCase1) ->
                //Compare the priorities
                //If matched priority is higher then stackExp operator (tryParseMathExpression)
                //Otherwise tryParseMathExpression remainingCase1 (Expression of stackExp stackOp expressionMatched) (opMatched)

                match (stackOp) with
                | Some someStackOp ->
                    if (getPriority opMatched) > (getPriority someStackOp) then
                        let resultToEnd, remainingCase11, isResultOpened, resultVariablesRef = tryParseMathExpression remainingCase1 (Some expression) (Some opMatched) (stackOp) (isOpened) variablesRefPassed
                        match resultToEnd with
                        | ExpressionParsingSuccess parsingResult ->
                            match parsingResult with
                            |Some someResultToEnd ->
                                tryParseMathExpression remainingCase11 (Some (Expression.BinaryExpression (someStackExp, someStackOp, someResultToEnd))) (None) (memoryOp) (isResultOpened) resultVariablesRef //(if isOpened then not isClosed else isOpened)
                            | None ->
                                (ExpressionParsingFailure (OperatorNotExpected remaining), input, isResultOpened, resultVariablesRef) //Problem with evaluating remainder of expression but now the program doesn't know what to do with the some stackExp and some stackOp
                        | ExpressionParsingFailure failure ->
                            (ExpressionParsingFailure failure, input , isResultOpened, resultVariablesRef)
                    else
                        let constMatchHandlingWithoutMemoryOp = fun() ->
                            let resultToEnd, remainingCase12, isResultOpened, resultVariablesRef = tryParseMathExpression remainingCase1 (None) (None) (Some opMatched) (isOpened) variablesRefPassed
                            match resultToEnd with
                            | ExpressionParsingSuccess parsingResult ->
                                match parsingResult with
                                |Some someResultToEnd ->
                                    tryParseMathExpression remainingCase12 (Some(Expression.BinaryExpression (Expression.BinaryExpression (someStackExp, someStackOp, expression) , opMatched, someResultToEnd))) (None) (memoryOp) (isResultOpened) resultVariablesRef //(if isOpened then not isClosed else isOpened)
                                | None ->
                                    (ExpressionParsingFailure (OperatorNotExpected remaining), input, isResultOpened, resultVariablesRef) //Problem with evaluating remainder of expression but now the program doesn't know what to do with the some stackExp and some stackOp
                            | ExpressionParsingFailure failure ->
                                (ExpressionParsingFailure failure, input, isResultOpened, resultVariablesRef)
                        match memoryOp with
                        | Some someMemoryOp ->
                            if (getPriority opMatched > getPriority someMemoryOp) then
                                constMatchHandlingWithoutMemoryOp()
                            else
                                (ExpressionParsingSuccess (Some(BinaryExpression (someStackExp, someStackOp, expression))),remaining, isOpened, variablesRefPassed)
                        | None ->
                            constMatchHandlingWithoutMemoryOp()
                | None ->
                    (ExpressionParsingFailure (MissingOperator input), input, false, variablesRefPassed) //Unexpected code during proper input This is possible during negative number input//When an expression(double/unaryExp/Variable) is matched there should either have been (some stackExp and some stackOp) or (None stackExp and None stackOp)//The first either or case fails here
                    
            | Success (Token.Bracket BracketClose, remainingCase2) ->
                match stackOp with
                    | Some someStackOp ->
                        tryParseMathExpression remaining (Some (Expression.BinaryExpression (someStackExp, someStackOp, expression))) (None) (memoryOp) (isOpened) variablesRefPassed
                    | _ -> 
                        (ExpressionParsingFailure (MissingOperator input), input, isOpened, variablesRefPassed)//Empty expressions
            | Failure _ ->
                match stackOp with
                | Some someStackOp ->
                    (ExpressionParsingSuccess (Some (Expression.BinaryExpression (someStackExp, someStackOp, expression))), remaining, isOpened, variablesRefPassed) //This could be the end of expression
                | None ->
                    (ExpressionParsingFailure (MissingOperator input), input, isOpened, variablesRefPassed)//Just returning the stackExp, but the program doesn't know what to do with the the parsed constMatched
            | _ ->
                (ExpressionParsingFailure (UnhandledInput input), input, false, variablesRefPassed)//Invalid input, an expression(double/unaryExp/Variable) should be followed by either bracket close or an arithmetic operator

    let rec getTheNextTermAsExpression = fun(expressionString:string) ->
        let nextToken = run (parseAToken()) expressionString
        match nextToken with
        | Success (Token.Bracket BracketOpen, remainingAfterBracketOpen) ->
            let resultToEndOfBracket, finalRemaining, isResultOpened, resultVariablesRef = tryParseMathExpression remainingAfterBracketOpen (None) (None) (None) (true) []
            if isResultOpened then
                (ExpressionParsingFailure (InsufficientParanthesis expressionString), expressionString, isResultOpened , resultVariablesRef)
            else
                match resultToEndOfBracket with
                | ExpressionParsingSuccess parsedResult ->
                    match parsedResult with
                    | Some someResultToOtherEnd ->
                        (ExpressionParsingSuccess (parsedResult), finalRemaining, isResultOpened, resultVariablesRef)
                    | None ->
                        (ExpressionParsingFailure (EmptyExpression expressionString), finalRemaining, isResultOpened, resultVariablesRef)
                | ExpressionParsingFailure failure ->
                    (ExpressionParsingFailure failure, expressionString, isResultOpened, resultVariablesRef)
        | Success (Token.DoubleConstant doubleConstant, remainingAfterDoubleConstant) ->
            (ExpressionParsingSuccess (Some (Expression.Constant doubleConstant)), remainingAfterDoubleConstant, false, [])
        | Success (Token.Variable variableName, remainingAfterVariable) ->
            if variables.ContainsKey(variableName) then
                let parserResult, remainingString, isResultOpened, _ = tryParseMathExpression variables.[variableName] (None) (None) (None) (false) []
                match parserResult with
                | ExpressionParsingSuccess expOpt ->
                    match expOpt with
                    | Some expression ->
                        (ExpressionParsingSuccess (expOpt), remainingAfterVariable, false, [variableName])
                    | None ->
                        (ExpressionParsingFailure (EmptyVariableExpression (sprintf "Variable %s does not have an expression" variableName)), expressionString, isOpened, [])
                | ExpressionParsingFailure failure ->
                    (ExpressionParsingFailure (VariableParsingFailed (sprintf "Unable to parse the expression related to the variable %s" variableName)), expressionString, isOpened, [])
            else
                (ExpressionParsingFailure (VariableDoesNotExists (sprintf "Variable %s existed during parsing doesn't exists during evaluation" variableName)), expressionString, isOpened, refVariables)
        | Success (Token.MasterKeywordVariable masterKeywordVariable, remainingAfterMasterVariable) ->
            if masterVariables.ContainsKey(masterKeywordVariable) then
                let parserResult, remainingString, isResultOpened, _ = tryParseMathExpression masterVariables.[masterKeywordVariable] (None) (None) (None) (false) []
                match parserResult with
                | ExpressionParsingSuccess expOpt ->
                    match expOpt with
                    | Some expression ->
                        (ExpressionParsingSuccess (expOpt), remainingAfterMasterVariable, false, [])
                    | None ->
                        (ExpressionParsingFailure (EmptyVariableExpression (sprintf "Master Variable %s does not have an expression" masterKeywordVariable)), expressionString, isOpened, refVariables)
                | ExpressionParsingFailure failure ->
                    (ExpressionParsingFailure (VariableParsingFailed (sprintf "Unable to parse the expression related to the master variable %s" masterKeywordVariable)), expressionString, isOpened, refVariables)
            else
                (ExpressionParsingFailure (VariableDoesNotExists (sprintf "Master Variable %s existed during parsing doesn't exists during evaluation" masterKeywordVariable)), expressionString, isOpened, refVariables)
        | Success (Token.UnaryOperator unaryOp, remainingAfterUnaryOp) ->
            let expressionTermResult, remainingAfterExprTerm, isOpenedAfterExprTerm, refVariablesInUnaryExp = getTheNextTermAsExpression(remainingAfterUnaryOp)
            match expressionTermResult with
            | ExpressionParsingSuccess expOpt ->
                match expOpt with
                | Some expression ->
                    let expressionToReturn = UnaryExpression (unaryOp, expression)
                    (ExpressionParsingSuccess (Some expressionToReturn), remainingAfterExprTerm, false, refVariablesInUnaryExp)
                | None ->
                    (ExpressionParsingFailure (EmptyVariableExpression (sprintf "Unary operator %A does not have a valid expression" unaryOp)), expressionString, isOpened, refVariables)
            | ExpressionParsingFailure failure ->
                (ExpressionParsingFailure (VariableParsingFailed (sprintf "Unable to parse the expression related to the Unary operator %A" unaryOp)), expressionString, isOpened, refVariables)
        | _ ->
            (ExpressionParsingFailure (InvalidExpressionTerm (sprintf "Unable to find a valid expression term in the input string : %A" expressionString)), expressionString, isOpened, refVariables)

    let result = run (parseAToken()) input
    match result with
    | Success (Token.Bracket BracketClose, remaining) ->
        //Validate whether stackOp is None - Pending
        if isOpened then
            (ExpressionParsingSuccess stackExp,remaining, not isOpened, refVariables)//Setting isOpened to false because BracketClose is found now
        else
            (ExpressionParsingFailure (TooManyParanthesis input) , input, isOpened, refVariables)//Not changing the state of isOpened as it's no more possible to close
    | Success (Token.Bracket _, _)//This would definitely have to be OpenBracket
    | Success (Token.DoubleConstant _, _)
    | Success (Token.MasterKeywordVariable _, _)
    | Success (Token.Variable _, _)
    | Success (Token.UnaryOperator _, _) ->
        let (resultToEndOfBracket, finalRemaining, isResultOpened, resultVariablesRef) = getTheNextTermAsExpression(input)
        let totalVariablesRef = (refVariables @ resultVariablesRef) |> List.distinct
        match resultToEndOfBracket with
        | ExpressionParsingSuccess parsedResult ->
            match parsedResult with
            | Some someResultToOtherEnd ->
                handleFoundExpression(someResultToOtherEnd, finalRemaining, totalVariablesRef)
            | None ->
                (ExpressionParsingFailure (EmptyExpression input), finalRemaining, isOpened, totalVariablesRef)//This is not expected when user enters multiplication even before brackets
        | ExpressionParsingFailure failure ->
            (ExpressionParsingFailure failure, input, isOpened, totalVariablesRef)
    | Success (Token.BinaryOperator opMatched, remaining) ->
        let opMatchedHandlingWithoutMemory = fun() ->
            match opMatched with
            | Minus ->
                match stackExp with
                | None ->
                    match stackOp with
                    | None ->
                        let (exprTermResult, remainingAfterExpressionTerm, isResultOpened, resultVariablesRef) = getTheNextTermAsExpression(remaining)
                        let totalVariablesRef = (refVariables @ resultVariablesRef) |> List.distinct
                        match exprTermResult with
                        | ExpressionParsingSuccess parsedResult ->
                            match parsedResult with
                            | Some someResultToOtherEnd ->
                                handleFoundExpression(BinaryExpression ( Expression.Constant (-1.0), BinaryOperator.Multiply, someResultToOtherEnd), remainingAfterExpressionTerm, totalVariablesRef)
                            | None ->
                                (ExpressionParsingFailure (EmptyExpression remaining), input, isOpened, totalVariablesRef)//This is not expected when user enters multiplication even before brackets
                        | ExpressionParsingFailure failure ->
                            (ExpressionParsingFailure failure, remaining, isOpened, totalVariablesRef)
                    | _ ->
                        (ExpressionParsingFailure (SequencingOfOperatorsNotAllowed input), input, isOpened, refVariables) // When a operator is matched stackOp should have been empty
                | Some someStackExp ->
                    match stackOp with
                    | None ->
                        (tryParseMathExpression remaining (stackExp) (Some opMatched) (memoryOp) (isOpened) refVariables)
                    | Some _ ->
                        (ExpressionParsingFailure (SequencingOfOperatorsNotAllowed input), input, isOpened, refVariables) //When an operator is matched, when stackExp is non-empty, stackOp should have been empty
            | _ ->
                match stackOp with
                | None ->
                    (tryParseMathExpression remaining (stackExp) (Some opMatched) (memoryOp) (isOpened) refVariables)
                | Some _ ->
                    (ExpressionParsingFailure (SequencingOfOperatorsNotAllowed input), input, isOpened, refVariables) //When an operator is matched, when stackExp is non-empty, stackOp should have been empty

        match memoryOp with
        | Some someMemoryOp ->
            if getPriority opMatched <= getPriority someMemoryOp then
                match stackExp with
                | Some someStackExp ->
                    (ExpressionParsingSuccess stackExp, input, isOpened, refVariables)
                | None ->
                    (ExpressionParsingFailure (OperatorNotExpected input), input , isOpened, refVariables)
            else 
                opMatchedHandlingWithoutMemory()
        | None ->
            opMatchedHandlingWithoutMemory()
    | Failure (_) ->
        if isOpened then
            (ExpressionParsingFailure (InsufficientParanthesis input), input, isOpened, refVariables)//Possibly throw an error here
        else
            match input.Trim().Length with
            | 0 ->
                //Operator in stackOp but incomplete expression is handled somewhere else
                match stackExp with
                | Some someStackExp ->
                    (ExpressionParsingSuccess stackExp,input, isOpened, refVariables)//Every expression is designed to terminate from here or from any of the None lines
                | None ->
                    (ExpressionParsingFailure (EmptyParanthesis input), input, isOpened, refVariables)
            | _ ->
                (ExpressionParsingFailure (UnrecognizedInput input), input, isOpened, refVariables)
