open System
open System.Runtime.InteropServices
open System.Threading
open System.Runtime.Remoting.Channels

// The ugly - algorithm complicated by the parsing
let uglyAddOneOrZero (input: string) = 
    match input with
    | _ when fst (Int32.TryParse input) -> int input + 1
    | _ -> 0

let (|ParsedInt|UnparsableInt|) input = 
    match input with
        | _ when fst (Int32.TryParse input) -> ParsedInt (int input)
        | _ -> UnparsableInt
    
let addOneOrZero (input: string) = 
    match input with
        | ParsedInt i -> i + 1 
        | UnparsableInt -> 0


uglyAddOneOrZero "42"
uglyAddOneOrZero "42a"
addOneOrZero "6"
addOneOrZero "foo"
