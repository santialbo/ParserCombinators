
module ParserCombinators.Main

open ParserCombinators.Core
open ParserCombinators.Json


[<EntryPoint>]
let main args =
    printfn "%A" (Run KeyNameParser "abc-def")
    0

