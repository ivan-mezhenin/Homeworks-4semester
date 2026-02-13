module Factorial

let factorial n  = 
    let rec loop n acc =  
        match n with
        | 0 | 1 -> Some acc
        | n when n < 0 -> None
        | _ -> loop (n - 1) (acc * n)
    
    loop n 1

