module BasicParsers
open ParserBuildingBlocks
open System
open System.Globalization

let digits = ['0'..'9']

let lowercaseLetters = ['a'..'z']
let uppercaseLetters = ['A'..'Z']                   

let charToList charList =
    String(List.toArray charList)

let pString str =
    str
    |> List.ofSeq
    |> List.map pChar
    |> sequence
    |>> charToList

let parseDigit = 
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

let parseLowerCaseLetter = (anyOf lowercaseLetters) <?> "Not a lowercase letter"
let parseUpperCaseLetter = (anyOf uppercaseLetters) <?> "Not a uppercase letter"

let spaces = (many ((pChar ' ') <|> (pChar '\n'))) <?> "whitespaces"

let parseDigitAsInt = 
    //(mapP parseDigit) |> int //This is also validfor the signature that was used for mapP
    parseDigit |>> int

let parseThreeDigitsAsStr = 
    parseDigit .>>. parseDigit .>>. parseDigit
    |>> (fun ((a,b), c) -> String [|a;b;c|])

let parseThreeDigitsAsInt =
    parseThreeDigitsAsStr
    |>> int

let charListToInt charList= 
    String(List.toArray charList) |> int       

let pInt = 
    many1 parseDigit
    |>> charListToInt

let pIntWithSign =
    (opt (pChar '-')) .>>. pInt

let oneOrMoreDigits =
    sepBy1 parseDigit (pChar ';')

let zeroOrMoreDigits =
    sepBy parseDigit (pChar ';')


let examplesForTestingParserBuildingBlocks =
    printfn "Testing a line"
    let stringInput = "ABC"
    let stringWithManyAs = "AAAABC"
    let stringWithManyAsButNotWithFirstChar = "BAAAABC"
    let numberInput = "123A"
    let negativeNumber = "-123A"
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

    let resultForManyAs = ((many parseA) |> run) stringWithManyAs

    let resultForMany1As = ((many1 parseA) |> run) stringWithManyAs

    let resultForIntParsing = (pInt |> run) numberInput

    let resultForIntWithNegParsing = (pIntWithSign |> run) negativeNumber

    let resultForOneOrMoreInts = (oneOrMoreDigits |> run) "1;2;3;4;5;"

    printfn "Run result : %A" resultForOneOrMoreInts