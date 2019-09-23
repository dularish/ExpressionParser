module BackTrackingModifiers
open ParserBuildingBlocks

let setLabelIfNoBacktracking parser newLabel =
    
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s ->
            Success s
        | Failure (oldLabel, err, pos) ->
            if(input.position.line <> pos.line || input.position.column <> pos.column) then
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
            if(input.position.line <> pos.line || input.position.column <> pos.column) then
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
    |> List.map pChar
    |> choiceWithoutBacktracking

let lazyOrElseWithoutBacktracking parser1 parser2 =
    let label = sprintf "lazy OrElse"
    let innerFn (input:InputState) =
        if (String.length (currentLine input)) = 0 then
            Failure (label, "Reached end of document", parserPositionFromInputState input)
        else
            let result1 = runOnInput (parser1()) input
            match result1 with
            | Success (matched1, remaining) ->
                Success (matched1, remaining)
            | Failure (errorLabel,err,pos) ->
                if(input.position.line <> pos.line || input.position.column <> pos.column) then
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