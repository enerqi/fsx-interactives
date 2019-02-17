let (|Mult3|_|) x = if x % 3 = 0 then Some () else None

let (|Mult5|_|) x = if x % 5 = 0 then Some () else None

[<Struct>]
type FizzBuzzMatch =
    | Fizz
    | Buzz
    | FizzBuzz

let fizzBuzzMatches start endInclusive = seq {
    for i in start .. endInclusive do
        match i with
        // struct (ValueTuple) and ValueOption
        // Note, ValueOptions have *just* one module function for them, so they are not yet very convenient
        | Mult3 & Mult5 -> yield struct (i, ValueSome FizzBuzz)
        | Mult3 -> yield struct (i, ValueSome Fizz)
        | Mult5 -> yield struct (i, ValueSome Buzz)
        | _ -> yield struct (i, ValueNone)
}

let showNumber i = printf "%d " i
for status in fizzBuzzMatches 0 100 do
    match status with
    | struct (i, ValueNone) -> showNumber i; printfn "----"
    | struct (i, ValueSome fbm) -> showNumber i; printfn "%A" fbm
