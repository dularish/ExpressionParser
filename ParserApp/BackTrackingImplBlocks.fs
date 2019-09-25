module BackTrackingModifiers
open ParserBuildingBlocks

//This module consists of only (eliminating) backtracking components
    //We sometimes do not want backtracking to happen in the cases like "parseForStatement <|> parseIfStatement"
        //In the above case, if "for" keyword is identified which means inputState is changed and not same as Initial condition, then the error message must be related to the 
            //parsing of ForStatement, it is of no use to the user to see error messages like "\"If\" keyword not found" when the user intended to use for statement

let isBackTrackingToBeOverrided = fun(inputState, failurePos) ->
    (((currentLine inputState) <> failurePos.currentLine) || (inputState.position.line <> failurePos.line) || (inputState.position.column <> failurePos.column))

let setLabelIfNoBacktracking parser newLabel =
    
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s ->
            Success s
        | Failure (oldLabel, err, pos) ->
            if(isBackTrackingToBeOverrided(input,pos)) then
                Failure (oldLabel, err, pos)
            else
                Failure(newLabel, err, pos)
    { parseFn = newInnerFn; label=newLabel}

let ( <?>! ) = setLabelIfNoBacktracking

let OrElseWithoutBacktracking parser1 parser2 =
    let label =
        sprintf "%s orElse %s" (getLabel parser1) (getLabel parser2)
    let innerFn input =
        let result1 = runOnInput parser1 input
        match result1 with
        | Success (matched1, remaining) ->
            Success (matched1, remaining)
        | Failure (label,err,pos) ->
            if(isBackTrackingToBeOverrided(input,pos)) then
                Failure (label, err, pos)
            else
                let result2 = runOnInput parser2 input
                match result2 with
                | Success (matched2,remaining2) ->
                    Success (matched2, remaining2)
                | Failure (label, err, pos) ->
                    Failure (label, err, pos)
    {parseFn=innerFn; label=label}

let (<|>!) = OrElseWithoutBacktracking

let choiceWithoutBacktracking listOfParsers =
    List.reduce (<|>!) listOfParsers

let anyOfWithoutBacktracking listOfChars =
    listOfChars
    |> List.map pchar
    |> choiceWithoutBacktracking

let lazyOrElseWithoutBacktracking parser1 parser2 =
    let label = sprintf "lazy OrElse"
    let innerFn (input:InputState) =
        let result1 = runOnInput (parser1()) input
        match result1 with
        | Success (matched1, remaining) ->
            Success (matched1, remaining)
        | Failure (errorLabel,err,pos) ->
            if(isBackTrackingToBeOverrided(input,pos)) then
                Failure (errorLabel,err,pos)
            else
                let result2 = runOnInput (parser2()) input
                match result2 with
                | Success (matched2,remaining2) ->
                    Success (matched2, remaining2)
                | Failure (errorLabel2,err2,pos2) ->
                    Failure (errorLabel2,err2,pos2)
    {parseFn= innerFn;label=label}

let (<^|^>!) = lazyOrElseWithoutBacktracking


let lazyChoiceWithoutBacktracking (listOfLazyParsers) =
    listOfLazyParsers
    |> List.reduce (fun x y ->
                        fun () -> lazyOrElseWithoutBacktracking x y)

let rec parserZeroOrMoreWithoutBacktracking parser input =

    let result1 = runOnInput parser input
    match result1 with
    | Failure (label, err, pos) ->
        if(isBackTrackingToBeOverrided(input,pos)) then
            Failure (label, err, pos)
        else
            Success ([], input)
    | Success (matched1, remainingInput1) ->
        match parserZeroOrMoreWithoutBacktracking parser remainingInput1 with
        | Success (subsequentValues, remainingInput2) ->
            Success (matched1 :: subsequentValues, remainingInput2)
        | Failure (label, err, pos) ->
            if(isBackTrackingToBeOverrided(input,pos)) then
                Failure (label, err, pos)
            else
                Success ([], input)

let manyWithoutBacktracking parser =
    let label = (sprintf "many of %s" (getLabel parser))
    let innerFn input = 
        (parserZeroOrMoreWithoutBacktracking parser input)

    {parseFn=innerFn; label = label}