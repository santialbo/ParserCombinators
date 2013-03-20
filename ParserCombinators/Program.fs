
module ParserCombinators.Main

open ParserCombinators.Core
open ParserCombinators.Json


         

[<EntryPoint>]
let main args =
    let a = ScapedString
    printfn "%A" (Run a @"\n\t\""hola")
    0

