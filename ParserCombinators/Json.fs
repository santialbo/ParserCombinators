
module ParserCombinators.Json

open ParserCombinators.Core

let AnsiLetterParser = CharParserF (fun c -> let c' = System.Char.ToLower c in c' >= 'a' && c <= 'z')

let DigitCharParser = CharParserF (fun c -> c >= '0' && c <= '9')

let KeyParser =
    parse {
        let! x = AnsiLetterParser <|> CharParser '_' 
        let! xs = Many (AnsiLetterParser <|> DigitCharParser <|> CharParser '_')
        return new System.String(x::xs |> List.toArray)
    }
    |> Between (CharParser '"') (CharParser '"') 
    
let ScapedString =
    let EscapedChars = [('t', '\t'); ('n', '\n'); ('r', '\r'); ('"', '"')] |> Map.ofList
    let NotEscapedCharParser = CharParserF (fun c -> not (c = '\\'))
    let EscapedCharsParser =
        parse {
            let! i = CharParser '\\'
            let! c = CharParserF EscapedChars.ContainsKey
            return EscapedChars.[c]
        }
    in parse {
        let! xs = Many (NotEscapedCharParser <|> EscapedCharsParser)
        return new System.String(xs |> List.toArray)
    }

