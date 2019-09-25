module ParserBuildingBlocks
open System
open System.Globalization


type ParserLabel = string
type ParserError = string

let eofString = "end of file"

type ParserPosition = {
    currentLine: string
    line: int
    column: int
    }
type Position = {
    line: int
    column:int
    }
type InputState = {
    lines: string[]
    position: Position
    }

let currentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then
        inputState.lines.[linePos]
    else
        eofString

let parserPositionFromInputState (inputState:InputState) = {
    currentLine = currentLine inputState
    line = inputState.position.line
    column = inputState.position.column
    }

type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Parser<'a> = {
    parseFn: (InputState -> Result<'a* InputState>)
    label: ParserLabel
    }



let initialPos = {line=0; column=0}

let incrCol pos =
    {pos with column=pos.column + 1}

let incrLine pos =
    {line=pos.line + 1; column=0}



let fromStr str = 
    if String.IsNullOrEmpty(str) then   
        {lines=[||]; position = initialPos}
    else
        let separators = [|"\r\n"; "\n"|]
        let lines = str.Split(separators, StringSplitOptions.None)
        {lines=lines; position=initialPos}



let nextChar input =
    let linePos = input.position.line
    let colPos = input.position.column

    if linePos >= input.lines.Length then
        input, None
    else
        let currentLine = currentLine input
        if colPos < currentLine.Length then
            let char = currentLine.[colPos]
            let newPos = incrCol input.position
            let newState = {input with position = newPos}
            newState, Some char
        else
            let char = '\n'
            let newPos = incrLine input.position
            let newState = {input with position = newPos}
            newState, Some char

let setLabel parser newLabel =
    
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s ->
            Success s
        | Failure (oldLabel, err, position) ->
            Failure(newLabel, err, position)
    { parseFn = newInnerFn; label=newLabel}

let ( <?> ) = setLabel

let satisfy predicate label =
    let innerFn input =
        let remainingInput, charOpt = nextChar input
        match charOpt with
        |Some char ->
            if predicate char then
                Success (char, remainingInput)
            else    
                let err = sprintf "Unexpected '%c'" char
                let pos = parserPositionFromInputState input
                Failure(label, err, pos)
        |None ->
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Failure (label, err, pos)
    {parseFn = innerFn; label = label}

let pchar charToMatch =
    let predicate ch = (ch = charToMatch)
    let label = sprintf "%c" charToMatch
    satisfy predicate label

let runOnInput parser inputState =
    parser.parseFn inputState

let run parser inputStr =
    runOnInput parser (fromStr inputStr)

//This works but reimplemented using bind
// let AndThen parser1 parser2 =
//     let innerFn input =
//         let result1 = run parser1 input
//         match result1 with
//         |Success (matched1, remaining1) ->
//             let result2 = run parser2 remaining1
//             match result2 with
//             | Success (matched2,remaining2) ->
//                 Success( (matched1, matched2) , remaining2)
//             | Failure err -> Failure err
//         | Failure err -> Failure err
//     Parser innerFn          

// let (.>>.) = AndThen     

let getLabel someParser =
    someParser.label

let OrElse parser1 parser2 =
    let label =
        sprintf "%s orElse %s" (getLabel parser1) (getLabel parser2)
    let innerFn input =
        let result1 = runOnInput parser1 input
        match result1 with
        | Success (matched1, remaining) ->
            Success (matched1, remaining)
        | Failure (label,err,pos) ->
            let result2 = runOnInput parser2 input
            match result2 with
            | Success (matched2,remaining2) ->
                Success (matched2, remaining2)
            | Failure (label, err, pos) ->
                Failure (label, err, pos)
    {parseFn=innerFn; label=label}

let (<|>) = OrElse

let choice listOfParsers =
    List.reduce (<|>) listOfParsers

let anyOf listOfChars =
    listOfChars
    |> List.map pchar
    |> choice

let bindP f parser =
    let label = "unknown"
    let innerFunc input = 
        let result1 = runOnInput parser input
        match result1 with
        | Failure (label,err, pos) ->
            Failure (label,err, pos)
        | Success (x, remainingInput1) ->
            let anotherParser = f x
            runOnInput anotherParser remainingInput1
    {parseFn= innerFunc; label = label}        

let (>>=) p f = bindP f p

//Notice how the function that we pass is always a one parameter function, what if it's a two or three parameter function? Well that's the limitation applyP solves
//Also notice how the function is applied only when it's a success
//This works but reimplemented using bind
// let mapP func parser =
//     let innerFunc input =
//         let result1 = run parser input
//         match result1 with
//         | Success (matched1, remaining1) ->
//             Success (func matched1, remaining1)
//         | Failure err ->
//             Failure err
//     Parser innerFunc       

let returnP x = 
    let innerFn input =
        Success (x, input)
    {parseFn=innerFn; label="something Insignificant"}

let returnFailure x = 
    let innerFn input =
        Failure (x)
    {parseFn= innerFn; label ="returnFailure"}

//Reimplemented using bind
let mapP f =
    bindP (f >> returnP)

let (<!>) = mapP  

//Reimplemented using bind
//Think how it gets executed
let AndThen parser1 parser2 =
    parser1 >>= (fun p1Result ->
    parser2 >>= (fun p2Result ->
        returnP(p1Result, p2Result)))
let (.>>.) = AndThen

//Reimplemented using bind
//Although it looks like the function accepts a single parameter, but it can be extended to use functions that take multiple parameters, see lift2 implementation
let applyP fP xP =
    fP >>= (fun(fResult) ->
    xP >>= (fun(xResult) ->
        returnP (fResult xResult)))

let ignoreLeft parser1 parser2 =
    parser1 .>>. parser2
    |> mapP (fun(a,b) -> b)

let (>>.) = ignoreLeft

let ignoreRight parser1 parser2 =
    parser1 .>>. parser2
    |> mapP (fun(a,b) -> a)  

let (.>>) = ignoreRight    

let between p1 p2 p3 =
    p1 >>. p3 .>> p2

//We want to put the mapping function after we get the desired parser
let ( |>> ) x f = mapP f x


//This works but reimplemented using bind
// let applyP fP xP =
//     fP .>>. xP
//     |>> (fun(f,x) ->f x)

let ( <*> ) = applyP

//Notice how two parameter function is lifted from normal world to parser world and two parameters in the parser world are passed to the function and combined
let lift2 f xP yP = 
    (returnP f) <*> xP <*> yP


//With the help of this helper, we are now capable of sequencing the results of each parser into a list
let rec sequence parsersList=
    let cons head tail = head :: tail

    let consP = lift2 cons

    match parsersList with
    | [] ->
        returnP []
    | head :: tail -> 
        consP head (sequence tail)

//Notice that the output is simply a tuple and we will be reusing this function for many and many1
let rec parserZeroOrMore parser input =

    let result1 = runOnInput parser input
    match result1 with
    | Failure (label, err, pos) ->
        ([], input)
    | Success (matched1, remainingInput1) ->
        let (subsequentValues, remainingInput2) =
            parserZeroOrMore parser remainingInput1
        (matched1 :: subsequentValues, remainingInput2)  


let many parser =
    let label = (sprintf "many of %s" (getLabel parser))
    let innerFn input = 
        Success (parserZeroOrMore parser input)

    {parseFn=innerFn; label = label}

//This works but reimplemented using bind
// let many1 parser = 
//     let innerFn input = 
//         let result1 = run parser input
//         match result1 with
//         | Failure err ->
//             Failure err
//         | Success (matched1, remainingInput1) ->
//             let (subsequentValues, remainingInput2) =
//                 parserZeroOrMore parser remainingInput1
//             Success (matched1:: subsequentValues, remainingInput2)
//     Parser innerFn     

//Reimplemented using bind
let many1 parser = 
    parser >>= (fun firstResult ->
    many parser >>= (fun restResultList ->
        returnP (firstResult::restResultList)))

let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

let sepBy1 parser sepParser =
    let sepThenParser = sepParser >>. parser

    parser .>>. (many sepThenParser)
    |>> (fun(x, xlist) -> x::xlist)

let sepBy parser sepParser =
    (sepBy1 parser sepParser) <|> (returnP [])

let generateResultText result =
    match result with
    | Success (value, input) ->
        sprintf "%A" value
    | Failure (label, err, parserPos) ->
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" err
        sprintf "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

let generateCustomErrorIndicator errorMessage parserPos =
    let errorLine = parserPos.currentLine
    let colPos = parserPos.column
    let linePos = parserPos.line
    let failureCaret = sprintf "%*s^%s" colPos "" errorMessage
    sprintf "Line:%i Col:%i \n%s\n%s" linePos colPos errorLine failureCaret

let printResult result =
    printfn "%s" (generateResultText result)