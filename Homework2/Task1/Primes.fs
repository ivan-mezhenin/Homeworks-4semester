module Primes

let primes =
    let rec sieve numbers = 
        seq {
            let p = Seq.head numbers
            yield p
            yield! sieve (Seq.filter (fun x -> x % p <> 0) (Seq.tail numbers)) 
        }

    sieve (seq {2 .. System.Int32.MaxValue})