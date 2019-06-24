module ParserBuildingBlocks
open System
open System.Globalization

type Result<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> Result<'a*string>)


let pChar charToMatch =
    let innerFunc stringInput =
        if String.IsNullOrEmpty stringInput then
            Failure "Empty input"
        else
            if stringInput.[0] = charToMatch then
                Success(stringInput.[0], stringInput.[1..])    
            else
                Failure (sprintf "Expecting %c but got %c" charToMatch stringInput.[0])

    Parser innerFunc         


let run parser input =
    let (Parser innerFunction) = parser
    innerFunction input

let AndThen parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        |Success (matched1, remaining1) ->
            let result2 = run parser2 remaining1
            match result2 with
            | Success (matched2,remaining2) ->
                Success( (matched1, matched2) , remaining2)
            | Failure err -> Failure err
        | Failure err -> Failure err
    Parser innerFn          

let (.>>.) = AndThen      

let OrElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Success (matched1, remaining) ->
            Success (matched1, remaining)
        | Failure err ->
            let result2 = run parser2 input
            match result2 with
            | Success (matched2,remaining2) ->
                Success (matched2, remaining2)
            | Failure err ->
                Failure err
    Parser innerFn        

let (<|>) = OrElse        

let ignoreLeft parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Success (matched1, remaining1) ->
            let result2 = run parser2 remaining1
            match result2 with
            | Success (matched2,remaining2) ->
                Success (matched2, remaining2)
            | Failure err ->
                Failure err
        | Failure err -> Failure err
    Parser innerFn     

let (>>.) = ignoreLeft

let ignoreRight parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input
        match result1 with
        | Success (matched1, remaining1) ->
            let result2 = run parser2 remaining1
            match result2 with
            | Success (matched2,remaining2) ->
                Success (matched1, remaining2)
            | Failure err ->
                Failure err
        | Failure err -> Failure err
    Parser innerFn     

let (.>>) = ignoreRight    


let choice listOfParsers =
    List.reduce (<|>) listOfParsers

let anyOf listOfChars =
    listOfChars
    |> List.map pChar
    |> choice

let digits = ['0'..'9']

let lowercaseLetters = ['a'..'z']
let uppercaseLetters = ['A'..'Z']

let transformErrorMessage parser errorMessage =
    let innerFn input =
        let result = (parser |> run) input
        match result with
        | Success (matched1, remaining1) ->
            Success (matched1, remaining1)
        | Failure err ->
            Failure errorMessage
    Parser innerFn                        

let parseDigit = 
    ((anyOf digits) |> transformErrorMessage) "Not a digit"

let parseLowerCaseLetter = (anyOf lowercaseLetters |> transformErrorMessage) "Not a lowercase letter"
let parseUpperCaseLetter = (anyOf uppercaseLetters |> transformErrorMessage) "Not a uppercase letter"

//Notice how the function that we pass is always a one parameter function, what if it's a two or three parameter function? Well that's the limitation applyP solves
let mapP parser func =
    let innerFunc input =
        let result1 = run parser input
        match result1 with
        | Success (matched1, remaining1) ->
            Success (func matched1, remaining1)
        | Failure err ->
            Failure err
    Parser innerFunc       

let (<!>) = mapP  

//We want to put the mapping function after we get the desired parser
let ( |>> ) f x = mapP f x

let returnP x = 
    let innerFn input =
        Success (x, input)
    Parser innerFn

let applyP fP xP =
    fP .>>. xP
    |>> (fun(f,x) ->f x)

let ( <*> ) = applyP

let parseDigitAsInt = 
    //(mapP parseDigit) |> int //This is also validfor the signature that was used for mapP
    parseDigit |>> int

let parseThreeDigitsAsStr = 
    parseDigit .>>. parseDigit .>>. parseDigit
    |>> (fun ((a,b), c) -> String [|a;b;c|])

let parseThreeDigitsAsInt =
    parseThreeDigitsAsStr
    |>> int

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

let charToList charList =
    String(List.toArray charList)

let pString str =
    str
    |> List.ofSeq
    |> List.map pChar
    |> sequence
    |>> charToList


let examplesForTestingParserBuildingBlocks =
    printfn "Testing a line"
    let stringInput = "ABC"
    let numberInput = "123A"
    let parseA = pChar 'A'
    let result = run parseA stringInput
    let parseB = pChar 'B'
    let parseC = pChar 'C'
    let parseAAndThenB = parseA .>>. parseB
    let parseAOrparseB = parseA <|> parseB
    let resultAB = run parseAAndThenB stringInput
    let resultAOrB = run parseAOrparseB stringInput

    let resultABWithoutA = (parseA >>. parseB |> run) stringInput
    let resultABWithoutB = (parseA .>> parseB |> run) stringInput

    let resultParseDigit = (parseDigitAsInt |> run) numberInput

    let resultParseUpperCase = (parseUpperCaseLetter |> run ) stringInput

    let parseAThenBThenC = parseA .>>. parseB .>>. parseC
    let resultParseABC = (parseAThenBThenC |> run) stringInput

    let resultParseThreeDigitsAsInt = (parseThreeDigitsAsInt |> run) numberInput

    let resultParseABCSeq = ((sequence [parseA;parseB;parseC]) |> run ) stringInput

    let resultParseABC = ((pString "ABC") |> run) stringInput

    printfn "Run result : %A" resultParseABC