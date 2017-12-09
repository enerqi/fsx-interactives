#r "packages/Hedgehog/lib/net452/Hedgehog.dll" 

open System

exception ExceptionNotANumber

let digits (numStr: string): int[] =
    let parsesOk, num = Int64.TryParse numStr    
    if parsesOk then
        let positiveNum = Math.Abs(num)
        let posString = positiveNum.ToString()
        
        seq posString |> Seq.map (fun c -> int(c.ToString())) |> Seq.toArray
    else
        // Array.empty
        raise ExceptionNotANumber
    
/// Visual divisibility tests as done by eye and from a string input and not by a computer in base 10 decimal
module IntegerDivisibilityTests =
    let divBy1 = true

    let divBy2 (n: string): bool =
        let ns = digits n
        let lastDigit = Array.last ns
        lastDigit % 2 = 0

    let isEven (x: int): bool = 
        x % 2 = 0

    let isOdd (x: int): bool = 
        not (isEven x)

    let lastTwoDigits (n: string): int = 
        let x = int(n)
        if x < 100 then  
            x
        else 
            let ns = digits n
            let digitsCount = ns.Length
            let tens = ns.[digitsCount - 2]
            let ones = ns.[digitsCount - 1]
            int(tens.ToString() + ones.ToString())
        
    let divBy3 (n: string): bool = 
        let digitsTotal = digits n |> Array.sum 
        digitsTotal % 3 = 0

    let divBy4 (n: string): bool = 
        (lastTwoDigits n) % 4 = 0

    let divBy5 (n: string): bool = 
        let lastDigit = digits n |> Array.last
        lastDigit = 5 || lastDigit = 0

    let divBy6 (n: string): bool = 
        divBy2 n && divBy3 n

    // 7 lacks an a consistent quick visual way to calculate divisibility if >= 1000
    
    let divBy8 (n: string): bool = 
        let x = int(n)
        if x < 100 then
            x % 8 = 0
        else
            let ns = digits n
            let hundreds = ns.[ns.Length - 3]            
            let lastTwoNum = lastTwoDigits n
            if isEven hundreds then
                lastTwoNum % 8 = 0
            else
                (lastTwoNum + 4) % 8 = 0

    let divBy9 (n: string): bool = 
        let digitsTotal = digits n |> Array.sum 
        digitsTotal % 9 = 0

    let divBy10 (n: string): bool = 
        let lastDigit = digits n |> Array.last 
        lastDigit = 0

    let divBy12 (n: string): bool = 
        divBy3 n && divBy4 n
       
module DivisibilityTestProperties = 
    open IntegerDivisibilityTests
    open Hedgehog

    let testRange = Range.linear -10000 10000
    let makeDivCheckProperty (n: int) (f: string -> bool): Property<unit> = 
        property {
            let! x = Gen.int testRange 
            let nString = x.ToString()
            return f nString = (x % n = 0)
        }

    let check2 = makeDivCheckProperty 2 divBy2
    let check3 = makeDivCheckProperty 3 divBy3
    let check4 = makeDivCheckProperty 4 divBy4    
    let check5 = makeDivCheckProperty 5 divBy5    
    let check6 = makeDivCheckProperty 6 divBy6    
    let check8 = makeDivCheckProperty 8 divBy8    
    let check9 = makeDivCheckProperty 9 divBy9    
    let check10 = makeDivCheckProperty 10 divBy10    
    let check12 = makeDivCheckProperty 12 divBy12
    let runs = 500<tests>
    
    Property.print' runs check2
    Property.print' runs check3
    Property.print' runs check4
    Property.print' runs check5
    Property.print' runs check6
    Property.print' runs check8
    Property.print' runs check9
    Property.print' runs check10
    Property.print' runs check12    
