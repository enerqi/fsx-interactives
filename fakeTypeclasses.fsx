// Simplest typeclass hack

type Semigroup<'a> = {
    append: 'a -> 'a -> 'a
}

type Monoid<'a> = {
    empty: 'a
    append: 'a -> 'a -> 'a
}

// In haskell these objects ('dicts' in haskell) are passed as hidden parameters to the relevant functions
let stringMonoid = {
    empty = ""
    append = (+)
}

let listMonoid = {
    empty = []
    append = List.append
}

let addMonoid = {
    empty = 0
    append = (+)
}

let andMonid = {
    empty = true
    append = (&&)
}

let appendElseEmpty (m: Monoid<'a>) condition (x: 'a) (y: 'a) = 
    if condition
    then m.append x y
    else m.empty

let appendIf (m: Semigroup<'a>) condition (x: 'a) (y: 'a) = 
    if condition
    then m.append x y
    else x

printfn "%A" (appendElseEmpty listMonoid true [1;2] [3;4])

// Follow up: type classery would allow monoid to inherit form semigroup
