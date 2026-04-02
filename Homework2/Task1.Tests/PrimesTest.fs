module PrimesTest

open NUnit.Framework
open FsUnit
open Primes

[<Test>]
let ``First prime is 2`` () =
    primes |> Seq.head |> should equal 2

[<Test>]
let ``First 10 primes are correct`` () =
    primes
    |> Seq.take 10
    |> Seq.toList
    |> should equal [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]

[<Test>]
let ``First 20 primes are correct`` () =
    primes
    |> Seq.take 20
    |> Seq.toList
    |> should equal 
        [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71]

[<Test>]
let ``100th prime is 541`` () =
    primes
    |> Seq.item 99
    |> should equal 541
