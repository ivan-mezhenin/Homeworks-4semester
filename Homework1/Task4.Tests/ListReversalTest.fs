module ListReversalTest

open NUnit.Framework
open FsUnit
open ListReversal  // предполагаем, что ваша функция в модуле ListReversal

[<Test>]
let ``Reverses a list with multiple elements`` () =
    reverse [1; 2; 3; 4; 5]
    |> should equal [5; 4; 3; 2; 1]

[<Test>]
let ``Reverses a list with strings`` () =
    reverse ["apple"; "banana"; "cherry"; "date"]
    |> should equal ["date"; "cherry"; "banana"; "apple"]

[<Test>]
let ``Reverses an empty list`` () =
    reverse []
    |> should equal []

[<Test>]
let ``Reverses a list with one element`` () =
    reverse [42]
    |> should equal [42]

[<Test>]
let ``Reverses a list with duplicate elements`` () =
    reverse [1; 2; 2; 3; 2; 1]
    |> should equal [1; 2; 3; 2; 2; 1]

[<Test>]
let ``Reverses a list of boolean values`` () =
    reverse [true; false; true; false]
    |> should equal [false; true; false; true]

[<Test>]
let ``Reverses a list of characters`` () =
    reverse ['a' .. 'e']
    |> should equal ['e'; 'd'; 'c'; 'b'; 'a']

[<Test>]
let ``Reverses a small list correctly`` () =
    reverse [10; 20]
    |> should equal [20; 10]

[<Test>]
let ``Works with negative numbers`` () =
    reverse [-5; -1; 0; 3; -10]
    |> should equal [-10; 3; 0; -1; -5]