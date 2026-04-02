module FibonacciTest

open NUnit.Framework
open FsUnit
open Fibonacci

[<Test>]
let ``fibonacciNumber 0 returns Some 0`` () =
    fibonacciNumber 0 |> should equal (Some 0)

[<Test>]
let ``fibonacciNumber 1 returns Some 1`` () =
    fibonacciNumber 1 |> should equal (Some 1)

[<Test>]
let ``fibonacciNumber 2 returns Some 1`` () =
    fibonacciNumber 2 |> should equal (Some 1)

[<Test>]
let ``fibonacciNumber 3 returns Some 2`` () =
    fibonacciNumber 3 |> should equal (Some 2)

[<Test>]
let ``fibonacciNumber 4 returns Some 3`` () =
    fibonacciNumber 4 |> should equal (Some 3)

[<Test>]
let ``fibonacciNumber 5 returns Some 5`` () =
    fibonacciNumber 5 |> should equal (Some 5)

[<Test>]
let ``fibonacciNumber 6 returns Some 8`` () =
    fibonacciNumber 6 |> should equal (Some 8)

[<Test>]
let ``fibonacciNumber 7 returns Some 13`` () =
    fibonacciNumber 7 |> should equal (Some 13)

[<Test>]
let ``fibonacciNumber 10 returns Some 55`` () =
    fibonacciNumber 10 |> should equal (Some 55)

[<Test>]
let ``fibonacciNumber 15 returns Some 610`` () =
    fibonacciNumber 15 |> should equal (Some 610)

[<Test>]
let ``fibonacciNumber 20 returns Some 6765`` () =
    fibonacciNumber 20 |> should equal (Some 6765)

[<Test>]
let ``negative number returns None`` () =
    fibonacciNumber -1  |> should equal None
    fibonacciNumber -5  |> should equal None
    fibonacciNumber -42 |> should equal None
