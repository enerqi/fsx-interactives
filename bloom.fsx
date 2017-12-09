// Probabilistic set 
// More memory efficient - array booleans
// Answers:
// No - definitely not there
// Yes - it might be in there, probably in there
// Typically members *cannot* be removed (without counting filter)
// The counting filter turns each bucket from a 1-bit bucket to n-bits, typically 3-4.
// However, if arithmetic overflow occurs, must leave it at max count all the time.
// * Really only makes sense where traditional hash tables take up too much room!
// E.g. is some url in our disk cache, where the number of different URLS for a cdn cache using a 
// normal hashtable could be super huge and so would exhaust memory if left to keep growing 
// How large to make the array, how many hashes, what hashes. That's the math part.

// This should be an ADT in a real impl
// List of hash functions paired with boolean array 
// The hash functions are used to flip the booleans inside the array 
// so upon using the hash it should have a way to return an index to the inside of the array
type Bloom = Bloom of (obj -> int) list * bool []  

type BloomContains = 
    | No
    | Probably

let mkBloom hashes size =
    Bloom
        (
           List.map (fun hsh -> fun v -> hsh v % size) hashes,
           Array.create size false
        )
        
let insert v (Bloom (hashes, data)) = 
    // Hash the value with each of the hash functions and set the corresponding index to true
    // Bloom filters embrace collisions
    Seq.iter (fun hsh -> data.[hsh v] <- true) hashes

let contains v (Bloom (hashes, data)) = 
    // If all the hash functions think it's in here, then probably in here
    // else definitely not 
    if List.forall (fun hsh -> data.[hsh v]) hashes
    then Probably
    else No

// nonsense hash function example
let hashIt seed (v: obj) = 
        // xor
    seed ^^^ v.GetHashCode()

let bloom = 
    mkBloom [hashIt 8298; hashIt 22; hashIt 897723; hashIt 92] 100

insert 42 bloom
insert 50 bloom

printfn "%A" (List.map (fun v -> v, contains v bloom) [0..60])
