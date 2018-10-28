// Types - int
// generic "elevated" types - Foo<int>


// Mappables (functors - beware Ocaml, C++ meanings)
// Map, fmap, lift, Select, <$>, <!>
// Lifting a function to the generic level
// (a->b)  applied to Foo<a> => Foo<b>
// We have one instance per generic type Foo<a> Bar<a> etc.
// Functor laws
// let id a = a
// map id === id
// map (f.g) === (map f) . (map g)
let mapOption f opt =
    match opt with
    | Some x -> Some (f x)
    | None -> None
Some 2 |> mapOption (fun x -> x + 1)

let rec mapList f list =
    match list with
    | [] -> []
    | head :: rest ->
        (f head) :: (mapList f rest)
[1;2;3] |> mapList (fun x -> x * x)

// Const Mappable
let mapOptionConst a opt =
    match opt with
    | None -> None
    | Some _ -> a


// return, unit, pure (applicative sense)
// Elevating *values*
// Often has literal/trivial syntax
let returnOption x = Some x
let returnList x = [x]


// Apply, ap, (<*>)
// application of generict elevated function Foo<(a->b)> to Foo<a>
let applyOption funcOption xOption =
    match funcOption, xOption with
    | Some f, Some x -> Some (f x)
    | _ -> None
// With infix <*>
// (Some add) <*> (Some 2) <*> (Some 3)


// liftN
// Mappable multi parameter
// lift1 === map
// lift2 (a->b->c) Foo<a> Foo<b> returning Foo<c>
// Neat implementation in terms of map/apply


// zip, zipWith, map2, (<*>)
// Combining 2 enumerables (maybe with a specific function)

// 'zipList' named version combining fList with xList
// Note List.apply is more likely to be implemented as a cross-product
// each function applied to each element


// bind, flatMapp, andThen, collect, SelectMany
// >>=, -<< (foo reversed parameters version)
// Composing simple to Generic type functions (a -> Foo<b>) "monadic"
// monadic functions (a -> Foo<b>) are chained with *bind*
// Think of monad as a transformation (i.e. sequencing one) that
// requires a generci type plus some relevant functions
// The notion of sequence with monads is because we chain functions
// that are dependendent upon the previous value
// With applicatives each Generic value is independent
// Applicatives are easier to do in parallel
let bindOpt (f: 'a -> 'b option) xOpt =
    match xOpt with
    | None -> None
    | Some x -> f x

let bindList (f: 'a -> 'b list) aList =  // List.collect
    [for x in aList do
     for y in f x do
     yield y] // flattens it out
// >>= pipes elevated values into these monadic function instead
// of writing out bindX elevatedX monadicF

// F# syntax sugar === let! in computation expressions
// initialExpression >>= (fun x ->
// expressionUsingX  >>= (fun y ->
// expressionUsingY  >>= (fun z ->
// x+y+z )))             // return
//
// computatonExpression {
//     let! x = initialExpression
//     let! y = expressionUsingX x
//     let! z = expressionUsingY y
//     return x + y + z
// }

// Applicative vs monadic style
// A: define actions up front, statically
// M: define only initial action, with others determined by
//    the output of the previous, flexible, less clear
type CustomerId = CustomerId of int
type EmailAddress = EmailAddress of string
type CustomerInfo = {
    id: CustomerId
    email: EmailAddress
}
type Result<'a> =
    | Success of 'a
    | Failure of string list
let createCustomerId id =
    if id > 0 then
        Success (CustomerId id)
    else
        Failure ["id must be positive"]
let createEmailAddress str =
    if System.String.IsNullOrEmpty(str) then
        Failure ["Email must not be empty"]
    elif str.Contains("@") then
        Success (EmailAddress str)
    else
        Failure ["Email must contain @-sign"]
module Result =
    let map f xResult =
        match xResult with
        | Success x -> Success (f x)
        | Failure errs -> Failure errs
    let retn x = Success x
    let apply fResult xResult =
        match fResult, xResult with
        | Success f, Success x ->
            Success (f x)
        | Failure errs, Success x ->
            Failure errs
        | Success _, Failure errs ->
            Failure errs
        | Failure errs1, Failure errs2 ->
            Failure (errs1 @ errs2)
    let bind f xResult =
        match xResult with
        | Success x -> f x
        | Failure errs -> Failure errs
// Applicative: validatons up front then combine results
// Monadic: one validation at a time chained
// More vs Less work and All errors visible vs some
// - Does it make sense to sequence the work?
// I'll note that the computation expression special syntax
// makes monadic more readable and probably common
let createCustomer customerId email =
    { id=customerId;  email=email }
let (>>=) x f = Result.bind f x
let (<!>) = Result.map
let (<*>) = Result.apply
let createCustomerResultApplicative id email =
    // We run both validations
    let idResult = createCustomerId id
    let emailResult = createEmailAddress email
    createCustomer <!> idResult <*> emailResult

let createCustomerResultMonadic id email =
    createCustomerId id >>= (fun customerId ->
        createEmailAddress email >>= (fun emailAddress ->
            let customer = createCustomer customerId emailAddress
            Success customer
        ))

