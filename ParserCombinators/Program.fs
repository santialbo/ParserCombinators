
module ParserCombinators.Main

open ParserCombinators.Core
open ParserCombinators.Json


         

[<EntryPoint>]
let main args =
    let a = FloatParser
    printfn "%A" (Run a @"-.2")
    printfn "%A" (Run a @"-1.2")
    printfn "%A" (Run a @"+.2")
    printfn "%A" (Run a @"+1.2")
    printfn "%A" (Run a @".2")
    printfn "%A" (Run a @"12.34")
    printfn "%A" (Run a @"12.34e12")
    printfn "%A" (Run a @"12.34e+12")
    printfn "%A" (Run a @"12.34e-12")
    0

