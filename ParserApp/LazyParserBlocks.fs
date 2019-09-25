module LazyParserBlocks

open FParsec
open MathematicalExpressionParser

//Lazy Parser blocks are developed so as to solve the issue of building nested self referencing parsers
    //For example in a mathematical expressions, a term can be an expression itself like (1 + (2 + (3 + 4))), it's not possible to know beforehand how many such nesting could be present
    //One of the solution to eliminate stack overflow exception while constructing nested self referencing parsers is introducing laziness,
        //where the parser object is not evaluated unless it's really required

//The implementation is based on the orElse implementation in the source code of FParsec (https://github.com/stephan-tolksdorf/fparsec/blob/master/FParsec/Primitives.fs)
let lazyOrElse (parser1: (unit -> Parser<'a>)) (parser2:(unit ->Parser<'a>)) :Parser<'a>=
    fun stream ->
        let stateTag = stream.StateTag
        let mutable reply = (parser1()) stream
        if reply.Status = Error && stateTag = stream.StateTag then
            let error = reply.Error
            reply <- parser2() stream
            if stateTag = stream.StateTag then
                reply.Error <- mergeErrors reply.Error error
        reply

let (<^|^>) = lazyOrElse

let lazyChoice (listOfLazyParsers) =
    listOfLazyParsers
    |> List.reduce (fun x y ->
                        fun () -> lazyOrElse x y)

//The implementation is based on the orElse implementation in the source code of FParsec (https://github.com/stephan-tolksdorf/fparsec/blob/master/FParsec/Primitives.fs)
let lazyAndThen (parser1: (unit -> Parser<'a>)) (parser2:(unit ->Parser<'b>)) =
    fun stream ->
        let reply1 = parser1() stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = parser2() stream
            let error = if stateTag1 <> stream.StateTag then reply2.Error
                        else mergeErrors reply1.Error reply2.Error
            let result = if reply2.Status = Ok then (reply1.Result, reply2.Result)
                         else Unchecked.defaultof<_>
            Reply(reply2.Status, result, error)
        else
            Reply(reply1.Status, reply1.Error)
let (.^>>^.) = lazyAndThen