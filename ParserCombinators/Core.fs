
module ParserCombinators.Core

type ParserResult<'a> =
    | Success of 'a * list<char> 
    | Failure
    
type Parser<'a> = list<char> -> ParserResult<'a>

/// Bind operator. Applies f to the result of parser p.
let (>>=) (p: Parser<'a>) (f: 'a -> Parser<'b>) : Parser<'b> =
    fun stream ->
        match p stream with
        | Success(x, rest) -> (f x) rest
        | Failure -> Failure

/// Always returns a Success with x as result
let preturn x : Parser<'a>=
    fun stream -> Success(x, stream)

/// Always fails.
let pzero : Parser<'a> =
    fun stream -> Failure

/// Computation expression builder.
type ParserBuilder() =
    member x.Bind(p, f) = p >>= f
    member x.Return(y) = preturn y

let parse = new ParserBuilder()

/// If p1 succeeds returns the result x.
let (>>%) p1 x : Parser<'b> =
    p1 >>= (fun _ -> preturn x)

/// Applies p1 and p2 returning the result of p2.
let (>>.) p1 p2 : Parser<'b> =
    p1 >>= (fun _ -> p2)

/// Applies p1 and p2 returning the result of p1.
let (.>>) p1 p2 : Parser<'a> =
    p1 >>= (fun x -> p2 >>% x)
    
/// Applies p1 and p2 returning as a result a tuple with both results.
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

/// Runs p as many times as posible, returning a list with the results.
let rec Many p : Parser<list<'a>> =
    parse {
        let! x = p
        let! xs = (Many p)
        return x :: xs
    } <|> preturn []

/// Runs p as many times as possible with at least one Succcess.
let Many1 p : Parser<list<'a>> =
    parse {
        let! x = p
        let! xs = (Many p)
        return x :: xs
    }

/// Runs p n times.
let rec Times n p: Parser<list<'a>> =
    parse {
        if n <= 0 then return []
        else
            let! x = p
            let! xs = (Times (n - 1) p)
            return x::xs
    }

/// Returns the first successful result of the given parser sequence.
let Choice ps : Parser<'a> =
    Seq.reduce (fun (p1: Parser<'a>) (p2: Parser<'a>) -> p1 <|> p2) ps
    
/// Runs the given parser against the given string.
let Run (p: Parser<'a>) (s: string) =
    p (Seq.toList s)

/// Parses the given character.
let CharParser c : Parser<char> =
    let p = function
        | x::xs when x = c -> Success(x, xs)
        | _ -> Failure
    in p
    
/// Parses characters which satisfy the given function.
let CharParserF f: Parser<char> =
    let p = function
        | x::xs when f x -> Success(x, xs)
        | _ -> Failure
    in p

/// Parses escaped characters in a string /\\[tnrbf"/\]/ and returns the appropiate characterñ   
let EscapedCharParser =
    let EscapedChars =
        [('t', '\t'); ('n', '\n'); ('r', '\r'); ('b', '\b'); ('f', '\f'); ('"', '"'); ('/', '/'); ('\\', '\\')]
        |> Map.ofList
    parse {
        let! i = CharParser '\\'
        let! c = CharParserF EscapedChars.ContainsKey |>> System.Char.ToLower
        return EscapedChars.[c]
    }

/// Helper function to build EscapedUtf_CharParser parsers
let EscapedUtfCharParser n =
    parse {
        let! i = CharParser '\\' >>. (CharParser 'u' <|> CharParser 'U')
        let! h =
            CharParserF (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <='f') || (c >= 'A' && c <='F'))
            |> Times 4
            |>> (fun cs -> new System.String(Array.ofList cs))
            |>> (fun s -> System.Int32.Parse(s, System.Globalization.NumberStyles.HexNumber))
            |>> System.Char.ConvertFromUtf32
            |>> Array.ofSeq
        return h.[0]
    }

/// Parses a string for escaped utf characters /\\[uU]\d{4,4}/
let EscapedUtf16CharParser = EscapedUtfCharParser 4
    
/// Parses a string for escaped utf characters /\\[uU]\d{8,8}/
let EscapedUtf32Parser = EscapedUtfCharParser 8
    
/// Parses the given string.
let StringParser (s: string) =
    let rec CharListParser (cs: list<char>) =
        match cs with
        | [] -> preturn []
        | c::cs' -> parse { let! c' = CharParser c
                            let! cs'' = CharListParser cs'
                            return c'::cs''
                          }
    CharListParser (Seq.toList s) |>> (fun cs -> new System.String(cs |> List.toArray))

/// Parses white space characters. Useful for skipping them.
let WhiteSpaceParser =
    [' '; '\t'; '\n'; '\r']
    |> List.map CharParser
    |> Choice
    |> Many

/// Parses integer number which match /[+-]?\d+/
let IntegerParser : Parser<int> =
    parse {
        let! s = (CharParser '+' <|> CharParser '-') <|> preturn '+'
        let! d = Many1 (CharParserF (fun c -> c >= '0' && c <= '9'))
        return int (new System.String(s::d |> List.toArray))
    }

/// Parses float numbers which match /[+-]?\d+(\.\d*)?([eE][+-]?\d+)?/
let FloatParser: Parser<float> =
    parse {
        let! s = (CharParser '+' <|> CharParser '-') <|> preturn '+'      // [+-]?
        let! l = Many (CharParserF (fun c -> c >= '0' && c <= '9'))       // \d+
        let! d = (parse {
            let! p = CharParser '.'                                       // (\.
            let! d = Many (CharParserF (fun c -> c >= '0' && c <= '9'))   //  \d*
            return p::d                                                   //     )?
        } <|> preturn [])
        let! e = (parse {
            let! e = CharParser 'e' <|> CharParser 'E'                    // ([eE]
            let! s = (CharParser '+' <|> CharParser '-') <|> preturn '+'  //   [+-]?
            let! x = Many1 (CharParserF (fun c -> c >= '0' && c <= '9'))  //   \d+
            return e::s::x                                                //         )?
        } <|> preturn [])
        return float (new System.String(s::(l @ d @ e) |> List.toArray))
    }

/// Applies the parsers popen, p and pclose in sequence. It returns the result of p.
let Between popen pclose p =
    popen >>. p .>> pclose
