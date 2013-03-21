
module ParserCombinators.Main

open ParserCombinators.Core
open ParserCombinators.Json

let text = """{
    "firstName": "John",
    "lastName": "Smith",
    "age": 25,
    "address": {
        "streetAddress": "21 2nd Street",
        "city": "New York",
        "state": "NY",
        "postalCode": 10021
    },
    "phoneNumber": [
        {
            "type": "home",
            "number": "212 555-1234"
        },
        {
            "type": "fax",
            "number": "646 555-4567"
        }
    ]
}"""


[<EntryPoint>]
let main args =
    printfn "%A" (Run JsonObjectParser text)
    0

