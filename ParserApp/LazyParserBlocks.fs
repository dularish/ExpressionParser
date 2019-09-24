module LazyParserBlocks

open ParserBuildingBlocks

//Lazy Parser blocks are developed so as to solve the issue of building nested self referencing parsers
    //For example in a mathematical expressions, a term can be an expression itself like (1 + (2 + (3 + 4))), it's not possible to know beforehand how many such nesting could be present
    //One of the solution to eliminate stack overflow exception while constructing nested self referencing parsers is introducing laziness,
        //where the parser object is not evaluated unless it's really required

let lazyOrElse parser1 parser2 =
    let label = sprintf "lazy OrElse"
    let innerFn (input:InputState) =
        let result1 = runOnInput (parser1()) input
        match result1 with
        | Success (matched1, remaining) ->
            Success (matched1, remaining)
        | Failure (errorLabel,err,pos) ->
            let result2 = runOnInput (parser2()) input
            match result2 with
            | Success (matched2,remaining2) ->
                Success (matched2, remaining2)
            | Failure (errorLabel2,err2,pos2) ->
                Failure (errorLabel2,err2,pos2)
    {parseFn= innerFn;label=label}

let (<^|^>) = lazyOrElse

let lazyChoice (listOfLazyParsers) =
    listOfLazyParsers
    |> List.reduce (fun x y ->
                        fun () -> lazyOrElse x y)

let lazyAndThen parser1Wrapped parser2Wrapped =
    let parser1 = parser1Wrapped()
    parser1 >>= (fun p1Result ->
    let parser2 = parser2Wrapped()
    parser2 >>= (fun p2Result ->
        returnP(p1Result, p2Result)))
let (.^>>^.) = lazyAndThen