/// Input: list of single digit integers. De-duplicate them, order them, then join the digits
/// into an integer value by combining the digits.
/// E.g. FormMin [3; 2; 1; 1; 2; 3] === 123
[<AutoOpen>]
module FormMinKata =
    open System
    [<Struct>]
    type private DigitConcatState = {
        PlaceValueIndex: int
        Accumulator: int
    }
    let FormMin (arr: int list): int =

        let uniqueDigits = List.distinct arr |> Array.ofList

        // The string approach is convenient as we don't need to reverse anything but it is computationally wasteful
        // making lots of single char strings, joining them then parsing that as an integer
        // let digitStrings = Array.map (fun d -> d.ToString()) uniqueDigits

        // Should be faster to sort a simple array despite the copying than an immutable list
        // An array sort is unstable, whereas a List sort would be stable
        Array.sortInPlace uniqueDigits |> ignore
        Array.Reverse uniqueDigits |> ignore
        // Possibly faster than sort then reverse, though less clear. For these tiny arrays won't matter much.
        // Array.sortInPlaceWith (fun n m -> -(compare n m)) uniqueDigits

        let accumulateDigit (state: DigitConcatState) (digit: int): DigitConcatState =
            let placeValue = pown 10 state.PlaceValueIndex
            let digitValue = placeValue * digit
            let newState: DigitConcatState = {
                PlaceValueIndex = state.PlaceValueIndex + 1
                Accumulator = state.Accumulator + digitValue
            }
            newState

        let concatState = Array.fold accumulateDigit {PlaceValueIndex = 0; Accumulator = 0} uniqueDigits
        concatState.Accumulator

/// Take every 2nd char from the string, then the other chars, that are not every 2nd char, and concat them
/// as new String. Do this n times!
/// If the input-string is null or empty return exactly this value!
/// If n is <= 0 then return the input text.
[<AutoOpen>]
module SimpleEncryptionAlternatingSplitKata =
    open System

    let encryptOnce (text: string): string =
        let indexedChars =
            text
            |> Seq.indexed
            |> Array.ofSeq  // There is no Seq.partition so we need an array
        let everyIndexedSecondChar, everyIndexedOtherChar =
            indexedChars
            |> Array.partition (fun (index, _) -> index % 2 = 1)
        let extractCharSeq (indexedChars: (int * char) []): seq<char> =
            indexedChars
            |> Seq.ofArray
            |> Seq.map (fun (_, char) -> char)

        // `string` would just call ToString on the Seq i.e. "Microsoft.Fsharp seq ... summary"
        let firstTextBlock = String.Concat(extractCharSeq everyIndexedSecondChar)
        let secondTextBlock = String.Concat(extractCharSeq everyIndexedOtherChar)
        String.Concat(firstTextBlock, secondTextBlock)

    let decryptOnce (text: string): string =
        // The other chars always get moved to the second half of the text string and
        // start at this index
        let splitIndex = text.Length / 2;
        let everySecondChar, everyOtherChar =
            text
            |> Array.ofSeq
            |> Array.splitAt splitIndex

        // We'd like to zip the arrays, but Array.zip always requires them to be the same length
        // Seq.zip allows different lengths but truncates the longer one
        // The same length requirement is true of `Array.mapi2` aswell
        // We can write a generic zip longest that puts default pad values in or just work from the
        // knowledge that there is only at most one extra character and execute different code branches
        let flatten (pairedChars: (char * char) []): seq<char> =
            // Tuples cannot be flattened with concat as they are not enumerable! They are an infinite combination
            // of types and don't have a clean length generic way to process them.
            seq { for (a, b) in pairedChars do yield! [|a; b|]}

        let flattenAndAppendExtra (extraChar: char) (pairedChars: (char * char) []): seq<char> =
            seq {
                for (a, b) in pairedChars do
                    yield! [|a; b|]
                yield extraChar
            }

        let decryptedChars =
            if everySecondChar.Length = everyOtherChar.Length then
                Array.zip everyOtherChar everySecondChar
                |> flatten
            else
                let lastChar = everyOtherChar.[everyOtherChar.Length - 1]
                let butLastEveryOtherChar = everyOtherChar.[0..everyOtherChar.Length - 2]
                Array.zip butLastEveryOtherChar everySecondChar
                |> flattenAndAppendExtra lastChar

        String.Concat(decryptedChars)

    let processTextWith (f: string -> string) (text: string) (nTimes: int): string =
        if not (String.IsNullOrEmpty text || nTimes <= 0) then
            seq { 1..nTimes }
            |> Seq.fold (fun textBlock _ -> f textBlock) text
        else
            text

    let encrypt (text: string) (n: int): string =
        processTextWith encryptOnce text n

    let decrypt (encryptedText: string) (n: int): string =
        processTextWith decryptOnce encryptedText n

[<AutoOpen>]
module StringRepeatKata =
    open System
    let repeatStr (n: int) (s: string): string =
        s
        |> Seq.replicate n
        |> String.Concat

/// You get the start number and the end number of a region and should return the count of all numbers
/// except numbers with a 5 in it. The start and the end number are both inclusive!
/// -5, 15, 25, 35, 45, 50, 55, 500, 5000 etc.
[<AutoOpen>]
module DontGiveMeFiveKata =
    let dontGiveMeFive (startValue: int) (endValue: int): int =
        let has5 n = string(n).Contains("5")  // the easy non-maths solution
        seq { startValue..endValue }
        |> Seq.sumBy (fun i -> if has5 i then 0 else 1)
