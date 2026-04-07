module Fibonacci

let fibonacciNumber n : int option =
    let rec fib prev curr step =
        match step with
        | 0 -> prev
        | _ -> fib curr (prev + curr) (step - 1)

    if n < 0 then None
    else
        match n with
        | 0 -> Some 0
        | _ -> Some (fib 0 1 n)