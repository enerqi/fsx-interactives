type Magma<'a> = 
  abstract member append : 'a -> 'a -> 'a

type Semigroup<'a> = 
  inherit Magma<'a>

type Monoid<'a> = 
  inherit Semigroup<'a>
  abstract member empty : 'a

type AddMonoid =
  new () = {}
  interface Monoid<int> with
    member this.empty = 0
    member this.append (a: int) (b: int) = a + b

let addMonoid = AddMonoid()    

type AndMonoid = 
  new () = {}
  interface Monoid<bool> with
    member this.empty = true
    member this.append (a: bool) (b: bool) = a && b

let andMonoid = AndMonoid()    

let appendElseEmpty (m: Monoid<'a>) condition (x :'a) (y: 'a) =
  if condition 
  then m.append x y 
  else m.empty

printfn "%A" (appendElseEmpty addMonoid true 1 2)
printfn "%A" (appendElseEmpty andMonoid true true false)    
