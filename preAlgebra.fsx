#r "packages/Hedgehog/lib/net452/Hedgehog.dll" 
#load "precomputedPrimes.fsx"

open System

exception NotANumberException

let digits (numStr: string): int[] =
    let parsesOk, num = Int64.TryParse numStr    
    if parsesOk then
        let positiveNum = Math.Abs(num)
        let posString = positiveNum.ToString()
        
        seq posString |> Seq.map (fun c -> int(c.ToString())) |> Seq.toArray
    else
        // Array.empty
        raise NotANumberException // or use a Result<int, ErrorType> type in a proper program
    
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

    // 7 lacks a consistent quick visual way to calculate divisibility if >= 1000
    
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

module Factors = 

    let factors (n: int32): int list = 
        let posNum = Math.Abs(n)
        let rec buildFactors (next: int) (sentinel: int) (factors: int list): int list = 
            if next >= sentinel then 
                factors |> List.sort
            else
                let nextIsFactor = posNum % next = 0
                if nextIsFactor then
                    let factorPairHighPart = posNum / next
                    let newFactors = 
                        // Avoid duplicates when we've reached the last factor pair
                        // e.g. (2, 2) is the last factor pair of 4
                        if factorPairHighPart <> next then  
                            (next :: factorPairHighPart :: factors)
                        else 
                            (next :: factors)
                    buildFactors (next + 1) factorPairHighPart newFactors
                else
                    buildFactors (next + 1) sentinel factors

        match posNum with 
        | 0 -> [0]
        | 1 -> [1]
        | _ -> buildFactors 1 posNum []

    let isComposite (n: int32): bool = 
        let factorsOf = factors n
        factorsOf.Length > 2  // more than 1 factor pair

    let isPrimeViaCompositesCheck (n: int32): bool = 
        // primes have exactly two factors. 1 is the factor of 1
        n > 1 && not (isComposite n)

    let isPrimeViaTrialDivision (n: int32): bool = 
        let testPosNum = Math.Abs(n)        
        // If it's a composite number then how large could the smallest divisor be?
        // composites (through prime factorisation) can be built out of some number of prime multiplications
        // e.g.  2 * 2 * 3 = 12. The largest prime that could be used in a multiplication is sqrt(n)
        // This primatlity test requires the prime numbers from 2 to sqrt(n), which is a somewhat odd given that
        // we are testing for primality and in theory do not know what the primes are.
        // There are 6542 pre-computed primes up to 2^16, which is ~25KB as a 32bit int or ~12KB as a short.
        // So this test is basically pointless for the pre-computed values up to 65K and allows calculating 
        // primes < 2^32 easily enough. 
        let trialDivision = (fun _ ->  
            assert (testPosNum > 4) 
            let highestPrimeCheck = int(Math.Sqrt(float testPosNum)) + 1
            PrecomputedPrimes.primes 
            |> Seq.ofArray
            |> Seq.map int
            |> Seq.takeWhile (fun p -> p <= highestPrimeCheck)
            |> Seq.forall (fun p -> testPosNum % p <> 0)
        )
        match testPosNum with
        | 0 -> false 
        | 1 -> false  // 1 is not prime (primes have 2 factors).
        | 2 -> true
        | 3 -> true
        | x when x % 2 = 0 -> false // 4 and all greater even numbers 
        | x -> trialDivision x  // 5+


module FactorProperties = 
    open Factors
    open Hedgehog
    let testRange = Range.linear -10000 10000
    let checkFactors = property {
        let! n = Gen.int testRange
        let factorsOf = factors n |> set
        let isFactorOf x = 
            n % x = 0
        return {1..n} |> Seq.forall (fun x -> (isFactorOf x) = factorsOf.Contains(x))
    }
    Property.print' 5000<tests> checkFactors

    let checkNumbersAboveTwoDifferInPrimeVsCompositeTest = property {
        let! n = Gen.int testRange
        let compositeIsNotPrime = (isComposite n) <> (isPrimeViaTrialDivision n)
        // 0 and 1 are not prime *and* not composite, otherwise every number should be differ 
        // in prime or composite predicates
        // we are treating negative numbers as complements of the positives
        return n = -1 || n = 0 || n = 1 || compositeIsNotPrime
    }
    Property.print' 5000<tests> checkNumbersAboveTwoDifferInPrimeVsCompositeTest

    let checkPrimalityAgainstPrecomputed = property {
        let! n = Gen.int (Range.constant 0 65536)
        let isPrimeOracle = Array.BinarySearch(PrecomputedPrimes.primes, uint16(n)) >= 0
        let isPrimeTrialDivision = isPrimeViaTrialDivision n
        return isPrimeOracle = isPrimeTrialDivision
    }
    Property.print' 5000<tests> checkPrimalityAgainstPrecomputed


