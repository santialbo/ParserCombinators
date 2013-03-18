
module ParserCombinators.Main

open ParserCombinators.Core

let letter = CharParserF (fun c ->
         let c' = System.Char.ToLower c;
         c' >= 'a' && c <= 'z')
let digit = CharParserF (fun c -> c >= '0' && c <= '9')
let symbol = CharParser '_' <|> CharParser '$'
let keyName = (letter <|> symbol) .>>. many (letter <|> digit <|> symbol) |>> (fun (x, xs) -> new System.String(x::xs |> List.toArray))

let key = keyName |> Between (CharParser '\"') (CharParser '\"') 
         

[<EntryPoint>]
let main args =
    let testp = key
    printfn "%A" (Run testp @"""test_name123""")
    printfn "%A" (Run testp @"""$test_name""")
    printfn "%A" (Run testp @"""_test_name""")
    0

