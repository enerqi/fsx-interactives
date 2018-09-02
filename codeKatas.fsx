open System

/// Input: list of single digit integers. De-duplicate them, order them, then join the digits
/// into an integer value by combining the digits.
/// E.g. FormMin [3; 2; 1; 1; 2; 3] === 123
[<AutoOpen>]
module FormMinKata =
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

    let private encryptOnce (text: string): string =
        let textChars = Array.ofSeq text
        let indexedChars = Array.indexed textChars
        let everySecondChar, everyOtherChar = Array.partition (fun (index, _) -> index % 2 = 0) indexedChars
        String.Concat(string(everySecondChar), string(everyOtherChar))

    let encrypt (text: string) (n: int): string =
        if String.IsNullOrEmpty text || n <= 0 then
            text
        else
            // Imperative solution
            // let mutable encrypted = text
            // for i = 1 to n do
            //     encrypted <- encryptOnce(encrypted)
            // encrypted
            [|1..n|] |> Array.fold (fun textBlock _ -> encryptOnce textBlock) text

    let decrypt (encryptedText: string) (n: int): string = ""

