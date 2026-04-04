module ParsingTreeTests

open NUnit.Framework
open FsUnit
open ParsingTree

[<Test>]
let ``evaluate Number returns the number itself`` () =
    evaluate (Number 42) |> should equal 42
    evaluate (Number 0) |> should equal 0
    evaluate (Number -7) |> should equal (-7)

[<Test>]
let ``evaluate Negative works correctly`` () =
    evaluate (Negative (Number 5)) |> should equal (-5)
    evaluate (Negative (Number (-10))) |> should equal 10

[<Test>]
let ``evaluate Add works correctly`` () =
    evaluate (Add (Number 3, Number 7)) |> should equal 10
    evaluate (Add (Number (-2), Number 5)) |> should equal 3

[<Test>]
let ``evaluate Sub works correctly`` () =
    evaluate (Sub (Number 10, Number 4)) |> should equal 6
    evaluate (Sub (Number 5, Number 8)) |> should equal (-3)

[<Test>]
let ``evaluate Mul works correctly`` () =
    evaluate (Mul (Number 4, Number 6)) |> should equal 24
    evaluate (Mul (Number (-3), Number 5)) |> should equal (-15)

[<Test>]
let ``evaluate Div works correctly`` () =
    evaluate (Div (Number 20, Number 4)) |> should equal 5
    evaluate (Div (Number 15, Number 3)) |> should equal 5

[<Test>]
let ``evaluate complex expression`` () =
    let expr = Add (Number 2, Mul (Number 3, Number 4))
    evaluate expr |> should equal 14

    let expr2 = Mul (Sub (Number 10, Number 2), Number 3)
    evaluate expr2 |> should equal 24

    let expr3 = Add (Div (Number 100, Number 5), Number 8)
    evaluate expr3 |> should equal 28

[<Test>]
let ``evaluate nested negative`` () =
    let expr = Negative (Add (Number 3, Number 5))
    evaluate expr |> should equal (-8)

[<Test>]
let ``evaluate should throw on division by zero`` () =
    let expr = Div (Number 10, Number 0)
    
    (fun () -> evaluate expr |> ignore)
    |> should throw typeof<System.Exception>

[<Test>]
let ``evaluate deep nested expression`` () =
    let expr = 
        Mul (
            Number 2, 
            Add (
                Number 3, 
                Mul (
                    Number 4, 
                    Sub (Number 5, Number 1)
                )
            )
        )
    
    evaluate expr |> should equal 38