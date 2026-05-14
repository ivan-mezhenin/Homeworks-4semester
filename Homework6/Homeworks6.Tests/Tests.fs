module Homeworks6.Tests

open NUnit.Framework
open FsUnit
open Homeworks6
open Calculate
open Rounding

[<Test>]
let ``calculate with valid numbers returns Some sum`` () =
    let result = calculate {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    }
    result |> should equal (Some 3.0)

[<Test>]
let ``calculate with invalid number returns None`` () =
    let result = calculate {
        let! x = "1"
        let! y = "\u042a"
        let z = x + y
        return z
    }
    result |> should equal None

[<Test>]
let ``calculate with all invalid returns None`` () =
    let result = calculate {
        let! x = "abc"
        let! y = "def"
        return x + y
    }
    result |> should equal None

[<Test>]
let ``calculate with mixed valid and invalid returns None`` () =
    let result = calculate {
        let! x = "3.14"
        let! y = "not a number"
        return x + y
    }
    result |> should equal None

[<Test>]
let ``rounding with precision 3 as in spec`` () =
    let result = rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    result |> should equal 0.048

[<Test>]
let ``rounding with precision 0 rounds to integer`` () =
    let result = rounding 0 {
        let! a = 1.7
        let! b = 2.3
        return a + b
    }
    result |> should equal 4.0

[<Test>]
let ``rounding intermediate result affects final result`` () =
    let result = rounding 1 {
        let! a = 1.44
        let! b = 2.17
        return a + b
    }
    result |> should equal 3.6

[<Test>]
let ``calculate supports return!`` () =
    let step s : float option =
        calculate {
            let! x = s
            return x
        }
    let result = calculate {
        return! step "42"
    }
    result |> should equal (Some 42.0)
