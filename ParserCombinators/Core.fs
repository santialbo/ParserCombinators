
module ParserCombinators.Core

type ParserResult<'a> =
    | Success of 'a * list<char> 
    | Failure
    
type Parser<'a> = list<char> -> ParserResult<'a>

let (>>=) (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    fun stream ->
        match p stream with
        | Success(x, rest) -> (f x) rest
        | Failure -> Failure

let preturn x : Parser<'a>=
    fun stream -> Success(x, stream)
    
let pzero : Parser<'a> =
    fun stream -> Failure
    
type ParserBuilder() =
    member x.Bind(p, f) = p >>= f
    member x.Return(y) = preturn y

let parse = new ParserBuilder()

let (>>%) p1 x : Parser<'b> =
    p1 >>= (fun _ -> preturn x)

let (>>.) p1 p2 : Parser<'b> =
    p1 >>= (fun _ -> p2)

let (.>>) p1 p2 : Parser<'a> =
    p1 >>= (fun x -> p2 >>% x)
    
let (.>>.) p1 p2: Parser<'a*'b> =
    p1 >>= (fun x -> p2 >>= (fun y -> preturn (x, y)))

let (<|>) (p1: Parser<'a>) (p2: Parser<'a>) : Parser<'a> =
    let p stream =
        match p1 stream with
        | Failure -> p2 stream
        | res -> res
    in p

let (|>>) p f : Parser<'b> =
    p >>= (fun x -> preturn (f x))
    
let rec Many p : Parser<list<'a>> =
    parse { let! x = p
            let! xs = (Many p)
            return x :: xs
           } <|> preturn []

let Many1 p : Parser<list<'a>> =
    parse { let! x = p
            let! xs = (Many p)
            return x :: xs
          }

let Choice ps : Parser<'a> =
    Seq.reduce (fun (p1: Parser<'a>) (p2: Parser<'a>) -> p1 <|> p2) ps
    
let Run (p: Parser<'a>) (s: string) =
    p (Seq.toList s)

let CharParser c : Parser<char> =
    let p = function
        | x::xs when x = c -> Success(x, xs)
        | _ -> Failure
    in p

let CharParserF f: Parser<char> =
    let p = function
        | x::xs when f x -> Success(x, xs)
        | _ -> Failure
    in p
    
let rec CharListParser (cs: list<char>) =
    match cs with
    | [] -> preturn []
    | c::cs' -> parse { let! c' = CharParser c
                        let! cs'' = CharListParser cs'
                        return c'::cs''
                      }
        
let rec CharListParser2 (cs: list<char>) =
    match cs with
    | [] -> preturn []
    | c::cs' ->
        (CharParser c) >>= (fun c' ->
            (CharListParser2 cs') >>= (fun cs'' -> preturn (c'::cs'')))

let StringParser (s: string) =
    CharListParser (Seq.toList s) |>> (fun cs -> new System.String(cs |> List.toArray))
    
let WhiteSpaceParser =
    [' '; '\t'; '\n'; '\r']
    |> List.map CharParser
    |> Choice

let IntegerParser : Parser<int> =
    parse {
        let! s = (CharParser '+' <|> CharParser '-') <|> preturn '+'
        let! d = Many1 (CharParserF (fun c -> c >= '0' && c <= '9'))
        return int (new System.String(s::d |> List.toArray))
    }

let FloatParser: Parser<float> =
    parse {
        let! s = (CharParser '+' <|> CharParser '-') <|> preturn '+'
        let! l = Many (CharParserF (fun c -> c >= '0' && c <= '9'))
        let! d = (parse {
            let! p = CharParser '.'
            let! d = Many (CharParserF (fun c -> c >= '0' && c <= '9'))
            return p::d
        } <|> preturn [])
        let! e = (parse {
            let! e = CharParser 'e' <|> CharParser 'E'
            let! s = (CharParser '+' <|> CharParser '-') <|> preturn '+'
            let! x = Many1 (CharParserF (fun c -> c >= '0' && c <= '9'))
            return e::s::x
        } <|> preturn [])
        return float (new System.String(s::(l @ d @ e) |> List.toArray))
    }
    
let Between p1 p2 p =
    p1 >>. p .>> p2
