module MinElementTest

open NUnit.Framework
open FsUnit
open MinElement

[<Test>]
let ``minElement returns None for empty list`` () =
    minElement [] |> should equal None

[<Test>]
let ``minElement returns Some value for single element list`` () =
    minElement [42] |> should equal (Some 42)

[<Test>]
let ``minElement finds minimum in positive numbers`` () =
    minElement [5; 3; 8; 1; 9] |> should equal (Some 1)

[<Test>]
let ``minElement finds minimum in negative numbers`` () =
    minElement [-2; -5; -1; -8; -3] |> should equal (Some -8)

[<Test>]
let ``minElement works with mixed positive and negative numbers`` () =
    minElement [10; -3; 0; 7; -15; 4] |> should equal (Some -15)

[<Test>]
let ``minElement finds minimum when it is first element`` () =
    minElement [1; 5; 8; 12] |> should equal (Some 1)

[<Test>]
let ``minElement finds minimum when it is last element`` () =
    minElement [10; 25; 7; 18; 3] |> should equal (Some 3)

[<Test>]
let ``minElement works with duplicate minimum values`` () =
    minElement [4; 2; 2; 5; 2; 7] |> should equal (Some 2)

[<Test>]
let ``minElement works with strings`` () =
    minElement ["banana"; "apple"; "cherry"; "date"] |> should equal (Some "apple")

[<Test>]
let ``minElement works with float numbers`` () =
    minElement [3.14; 2.71; 1.618; 0.577] |> should equal (Some 0.577)

[<Test>]
let ``minElement returns correct result for two elements`` () =
    minElement [10; 5] |> should equal (Some 5)
    minElement [5; 10] |> should equal (Some 5)