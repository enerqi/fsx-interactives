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
        // Todo: System.String would be just as good (distinguishing from the `String` module)
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


/// A child is playing with a ball on the nth floor of a tall building. The height of this floor, h, is known.
/// He drops the ball out of the window. The ball bounces (for example), to two-thirds of its height (a bounce of 0.66).
/// His mother looks out of a window 1.5 meters from the ground.
/// How many times will the mother see the ball pass in front of her window (including when it's falling and bouncing?
/// The ball can only be seen if the height of the rebounding ball is stricty greater than the window parameter.
[<AutoOpen>]
module BouncingBallsKata =
    let rec private calcBounces (h: float) (bounce: float) (window: float): int =
        // At the start of this function the ball is always at full height for the current drop/bounce
        if h > window then
            let reboundHeight = bounce * h
            let windowPasses =
                if reboundHeight > window then
                    2
                else
                    1
            windowPasses + calcBounces reboundHeight bounce window
        else
            0
    let bouncingBall (h: float) (bounce: float) (window: float): int =
        if h > 0.0 && bounce > 0.0 && bounce < 1.0 && window < h then
            calcBounces h bounce window
        else
            -1


[<AutoOpen>]
module DisemvowelKata =
    open System
    let disemvowel (s: string): string =
        let isNotVowel character =
            let vowels = [|'a'; 'e'; 'i'; 'o'; 'u'; 'A'; 'E'; 'I'; 'O'; 'U'|]
            not <| Array.contains character vowels

        s
        |> Array.ofSeq
        |> Array.filter isNotVowel
        |> String.Concat


/// Take an integer n (n >= 0) and a digit d (0 <= d <= 9) as an integer. Square all numbers k (0 <= k <= n)
/// between 0 and n. Count the numbers of digits d used in the writing of all the k**2.
/// Call nb_dig (or nbDig or ...) the function taking n and d as parameters and returning this count.
[<AutoOpen>]
module CountDigitsKata =
    let nbDig (n: int) (d:int): int =

        let rec digitCount' (accumulator: int) (i: int): int =
            let rightMostDigitMatches = (i % 10) = d
            let isLastDigit = i < 10
            match (isLastDigit, rightMostDigitMatches) with
            | true, true -> 1 + accumulator
            | true, false -> accumulator
            | false, true -> digitCount' (1 + accumulator) (i / 10)
            | false, false -> digitCount' accumulator (i / 10)

        let digitCount k = digitCount' 0 k
        let ks = seq { for i in 0 .. n -> i * i}
        Seq.sumBy digitCount ks

/// Basically like a Fibonacci, but summing the last 3 (instead of 2) numbers of the sequence to generate the next
/// signature [1,1,1] then [1, 1 ,1, 3, 5, 9, 17, 31, ...]
/// But what if we started with [0, 0, 1] as a signature?
/// Given a signature array/list, returns the first n elements - signature included of the so seeded sequence.
[<AutoOpen>]
module TribonacciKata =
    let tribonacci (signature: int list) (n: int): int list =

        if n <= 3 then
            signature |> List.take n
        else
            let startTriple =
                match signature with
                | [a; b; c] -> (a, b, c)
                | _ -> failwith "signature should be a triple"

            let nextTrib = function (a, b, c) -> Some(a + b + c, (b, c, a + b + c))

            signature @ (startTriple |> Seq.unfold nextTrib |> Seq.take (n - 3) |> List.ofSeq)

[<AutoOpen>]
module TenMinuteWalk =
    let isValidWalk (walk: char list): bool =

        if List.length walk <> 10 then
          false
        else
          let move (positionState: int * int) (direction: char): int * int =
              let x, y = positionState
              match direction with
              | 'n' -> (x, y + 1)
              | 's' -> (x, y - 1)
              | 'e' -> (x + 1, y)
              | 'w' -> (x - 1, y)
              | _ -> failwith "illegal walk direction"

          let startPosition = (0, 0)
          let walkFinishPosition = walk |> List.fold move startPosition
          walkFinishPosition = startPosition

/// rgb values constrained to 0-255 then converted to an uppercase hexadecimal string
[<AutoOpen>]
module RGBToHex =
    open System

    let rgb (r: int) (g: int) (b: int): string =

        let normalise = function
            | n when n < 0 -> 0
            | n when n > 255 -> 255
            | n -> n

        let hex (n: int): string =
            sprintf "%02X" (normalise n)  // X=hex, 0 means zero pad, 2 padding length

        (hex r) + (hex g) + (hex b)

// How many iterations of the Leibniz formula for PI does it take to
// achieve a certain accuracy (the diff between our calculation and Math.PI is < epsilon)
// π/4 ~= 1 - (1/3 + 1/5) - (1/7 + 1/9) - (1/11 + 1/13) -...
[<AutoOpen>]
module PIApproxKata =
    open System
    let iterPi (epsilon: double): int * double =

        let PI: double = 3.14159265358979323846  // Math.PI is only a float
        // *The fully imperative soluton*
        //
        // let mutable piApprox = 1.0
        // let mutable iterationsCount = 1  // started with 1.0 already
        // let mutable seriesSum = 1.0
        // let mutable denominator = 1
        // let mutable isAdditionOperation = false
        // while Math.Abs(piApprox - PI) >= epsilon do
        //     denominator <- denominator + 2
        //     let next = 1.0 / double(denominator)
        //     if isAdditionOperation then
        //         seriesSum <- seriesSum + next
        //     else
        //         seriesSum <- seriesSum - next
        //     isAdditionOperation <- not isAdditionOperation
        //     iterationsCount <- iterationsCount + 1
        //     piApprox <- seriesSum * 4.0
        // (iterationsCount, piApprox)

        let rec nextIteration iterations piApproximation seriesSum termDenominator isAddition =
            if Math.Abs(piApproximation - PI) < epsilon then
                (iterations, piApproximation)
            else
                let denominator = termDenominator + 2
                let termValue = 1.0 / double(denominator)
                let nextSeriesSum =
                    if isAddition then
                        seriesSum + termValue
                    else
                        seriesSum - termValue
                let piApprox = nextSeriesSum * 4.0
                nextIteration (iterations + 1) piApprox nextSeriesSum denominator (not isAddition)

        let initialSeriesSum = 1.0  // counts as an iteration
        let initialIterationsCount = 1
        let initialDenominator = 1
        let isAddition = false
        nextIteration initialIterationsCount 0.0 initialSeriesSum initialDenominator isAddition

    // Improvements? Though likely not more performant in any way:
    // You don't have do collect all of a seq and turn it into a collection or display
    // it all. Running find/tryFind/take n etc. can end it early
    // Seq.initInfinite is useful as a general 'enumerate'
    // Seq.unfold
    // seq custom expression
    // `sign * -1` is better than isAddition or a seq of alterning -1/d ... +1/d+n ...


/// Given two arrays of strings a1 and a2 return a sorted array r in lexicographical order
/// of the strings of a1 which are substrings of strings of a2.
/// Beware: result must be without duplicates.
[<AutoOpen>]
module WhichAreInKata =
    let inArray (a1: string list) (a2: string list) =

        let contains (s1: string) (s2: string) =
            s2.Contains(s1)

        [for s in a1 do
         if a2 |> List.exists (contains s) then yield s]
        |> List.distinct
        |> List.sort

[<AutoOpen>]
module LostSheepKata =
    let lostSheep (fridaysCount:int array) (saturdaysCount:int array) (totalSheep:int) : int =
        totalSheep - (Array.sum fridaysCount + Array.sum saturdaysCount)


/// Write a function that receives two strings and returns n, where n is equal
/// to the number of characters we should shift the first string forward (right) to match
/// the second.
[<AutoOpen>]
module CalcStringRotation =
    open System

    let private rotateString (rightRotations: int) (s: string): string =
        if s.Length < 2 || rightRotations = 0 then
            s
        else
            // ToCharArray then modifying in place and back to String is 2 copies so just use the substring functionality
            let shifts =
                if rightRotations >= s.Length then
                    rightRotations % s.Length
                else
                    rightRotations
            let d = s.Length - shifts
            s.Substring(d) + s.Substring(0, d)

    let ShiftedDiff (first : string) (second : string): int =
        seq { for i in 0..first.Length -> (i, rotateString i first) }
        |> Seq.tryFind (fun (_, rotatedString) -> rotatedString = second)
        |> Option.map (fun (rotationsCount, _) -> rotationsCount)
        |> Option.defaultValue -1
