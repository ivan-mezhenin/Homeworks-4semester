module SquareTests

open NUnit.Framework
open FsUnit
open Square

[<Test>]
let ``squareLines returns empty list for n <= 0`` () =
    squareLines 0 |> should be Empty
    squareLines (-1) |> should be Empty
    squareLines (-5) |> should be Empty

[<Test>]
let ``squareLines returns correct square for n = 1`` () =
    squareLines 1 |> should equal ["*"]

[<Test>]
let ``squareLines returns correct square for n = 2`` () =
    squareLines 2 |> should equal ["**"; "**"]

[<Test>]
let ``squareLines returns correct square for n = 3`` () =
    squareLines 3 |> should equal 
        ["***"; 
         "* *"; 
         "***"]

[<Test>]
let ``squareLines returns correct square for n = 4`` () =
    squareLines 4 |> should equal 
        ["****"; 
         "*  *"; 
         "*  *"; 
         "****"]

[<Test>]
let ``squareLines returns correct square for n = 5`` () =
    squareLines 5 |> should equal 
        ["*****"; 
         "*   *"; 
         "*   *"; 
         "*   *"; 
         "*****"]

[<Test>]
let ``squareLines produces correct number of lines`` () =
    squareLines 6 |> List.length |> should equal 6
    squareLines 10 |> List.length |> should equal 10

[<Test>]
let ``squareLines produces lines of correct length`` () =
    let lines = squareLines 7
    lines |> List.iter (fun line -> line.Length |> should equal 7)

[<Test>]
let ``first and last lines are full of asterisks`` () =
    let lines = squareLines 5
    lines.[0] |> should equal "*****"
    lines.[lines.Length - 1] |> should equal "*****"

[<Test>]
let ``middle lines start and end with asterisk`` () =
    let lines = squareLines 5
    let middleLines = lines.[1..3]
    
    middleLines |> List.iter (fun line ->
        line.[0] |> should equal '*'
        line.[line.Length - 1] |> should equal '*'
    )