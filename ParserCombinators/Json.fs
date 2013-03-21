
module ParserCombinators.Json

type JsonObject = Map<string, JsonValue>

and JsonValue =
    | Null
    | Boolean of bool
    | String of string
    | Number of float
    | Array of list<JsonValue>
    | Object of JsonObject

open ParserCombinators.Core


let KeyNameParser =
    let AnsiLetterParser = CharParserF (fun c -> let c' = System.Char.ToLower c in c' >= 'a' && c <= 'z')
    let DigitCharParser = CharParserF (fun c -> c >= '0' && c <= '9')
    parse {
        let! x = AnsiLetterParser <|> CharParser '_' 
        let! xs = Many (AnsiLetterParser <|> DigitCharParser <|> CharParser '_')
        return new System.String(x::xs |> List.toArray)
    }
    
    
let EscapedStringParser =
    let EscapedChars =
        [('t', '\t'); ('n', '\n'); ('r', '\r'); ('b', '\b'); ('f', '\f'); ('"', '"'); ('/', '/'); ('\\', '\\')]
        |> Map.ofList
        
    let NotEscapedCharParser = CharParserF (fun c -> not (c = '\\' || c = '"'))
    
    let EscapedUnicodeParser =
        parse {
            let! i = CharParser '\\' >>. (CharParser 'u' <|> CharParser 'U')
            let! h =
                CharParserF (fun c -> (c >= '0' && c <= '9') && (c >= 'a' && c <='f') && (c >= 'A' && c <='F'))
                |> Times 4
                |>> (fun cs -> new System.String(Array.ofList cs))
                |>> (fun s -> System.Int32.Parse(s, System.Globalization.NumberStyles.HexNumber))
                |>> System.Char.ConvertFromUtf32
                |>> Array.ofSeq
            return h.[0]
        }
    
    let EscapedCharsParser =
        parse {
            let! i = CharParser '\\'
            let! c = CharParserF EscapedChars.ContainsKey |>> System.Char.ToLower
            return EscapedChars.[c]
        }
        
    parse {
        let! xs = Many (NotEscapedCharParser <|> EscapedCharsParser)
        return new System.String(xs |> List.toArray)
    }
    |> Between (CharParser '"') (CharParser '"')

let BooleanParser = (StringParser "true" >>% true) <|> (StringParser "false" >>% false) |>> Boolean 

let NullParser = StringParser "null" >>% Null

let mutable ValueParser = preturn Null
    
let KeyValueParser =
    Between (CharParser '"') (CharParser '"') KeyNameParser .>> WhiteSpaceParser
    .>> CharParser ':' .>> WhiteSpaceParser
    .>>. ValueParser

let ArrayParser =
    parse {
        let! x = ValueParser .>> WhiteSpaceParser
        let! xs = Many (CharParser ','  >>. WhiteSpaceParser >>. ValueParser .>> WhiteSpaceParser)
        return x::xs
    } <|> preturn []
    |> Between (CharParser '[' .>> WhiteSpaceParser) (CharParser ']')
    |>> Array

let ObjectParser =
    parse {
        let! x = KeyValueParser .>> WhiteSpaceParser
        let! xs = Many (CharParser ','  >>. WhiteSpaceParser >>. KeyValueParser .>> WhiteSpaceParser)
        return x::xs
    } <|> preturn []
    |>> Map.ofList
    |> Between (CharParser '{' .>> WhiteSpaceParser) (CharParser '}')
    |>> Object 

ValueParser <-
    Choice [BooleanParser;
            NullParser;
            FloatParser |>> Number;
            EscapedStringParser |>> String;
            ArrayParser;
            ObjectParser] .>> WhiteSpaceParser
