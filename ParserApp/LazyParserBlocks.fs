module LazyParserBlocks

open ParserBuildingBlocks

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