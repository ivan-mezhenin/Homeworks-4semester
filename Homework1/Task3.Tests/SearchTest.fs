module SearchTests

open NUnit.Framework
open FsUnit
open Search

[<Test>]
let ``Search for second element in int array`` () =
    search [10; 20; 30; 40; 50] 30
    |> should equal (Some 2)

[<Test>]
let ``Search for zero element in int array `` () =
    search [7; 8; 9; 10] 7
    |> should equal (Some 0)

[<Test>]
let ``Search for last element in string array`` () =
    search ["cat"; "dog"; "bird"; "fish"] "fish"
    |> should equal (Some 3)

[<Test>]
let ``Element not in array should return None`` () =
    search [1..10] 777
    |> should equal None

[<Test>]
let ``Empty list should return None`` () =
    search [] 42
    |> should equal None

[<Test>]
let ``List with one element`` () =
    search [100] 100
    |> should equal (Some 0)

[<Test>]
let ``Test for search first occurrence of number`` () =
    search [1; 2; 2; 2; 3; 2] 2
    |> should equal (Some 1)

[<Test>]
let ``String array`` () =
    search ["apple"; "banana"; "cherry"; "date"] "cherry"
    |> should equal (Some 2)

[<Test>]
let ``Search for element in option int list`` () =
    search [None; Some 5; None] (Some 5)
    |> should equal (Some 1)

[<Test>]
let ``Search for element in bool list`` () =
    search [true; false] false
    |> should equal (Some 1)