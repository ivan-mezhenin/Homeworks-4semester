module FactorialTests

open NUnit.Framework
open FsUnit
open Factorial

[<Test>]
let ``Factorial of 0 should return Some 1`` () =
    factorial 0 |> should equal (Some 1)

[<Test>]
let ``Factorial of 1 should return Some 1`` () =
    factorial 1 |> should equal (Some 1)

[<Test>]
let ``Factorial of 5 should return Some 120`` () =
    factorial 5 |> should equal (Some 120)

[<Test>]
let ``Factorial of negative number should return None`` () =
    factorial -1 |> should equal None
    factorial -5 |> should equal None

[<Test>]
let ``Factorial of 3 should return Some 6`` () =
    factorial 3 |> should equal (Some 6)

[<Test>]
let ``Factorial of 10 should return Some 3628800`` () =
    factorial 10 |> should equal (Some 3628800)

[<Test>]
let ``Factorial of 1000 should return Some big number`` () =
    match factorial 1000 with
    | Some _ -> Assert.Pass("Работает!")
    | None -> Assert.Fail("Должен быть Some")

[<Test>]
let ``Factorial of very large negative should be None`` () =
    factorial -1000000 |> should equal None