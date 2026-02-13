module NumberOfDegreesTests

open NUnit.Framework
open FsUnit
open NumberOfDegrees

[<TestCase(0, 0, [|1|])>]
[<TestCase(1, 0, [|2|])>]
[<TestCase(2, 0, [|4|])>]
[<TestCase(3, 0, [|8|])>]
[<TestCase(0, 1, [|1;2|])>]
[<TestCase(0, 2, [|1;2;4|])>]
[<TestCase(0, 3, [|1;2;4;8|])>]
[<TestCase(1, 1, [|2;4|])>]
[<TestCase(1, 2, [|2;4;8|])>]
[<TestCase(1, 3, [|2;4;8;16|])>]
[<TestCase(2, 2, [|4;8;16|])>]
[<TestCase(2, 3, [|4;8;16;32|])>]
[<TestCase(3, 3, [|8;16;32;64|])>]
let ``degrees should return correct powers of two`` (n, m, expected) =
    degrees n m |> should equal (List.ofArray expected)

[<Test>]
let ``degrees with m = 0 returns single element list`` () =
    degrees 5 0 |> should equal [32]
    degrees 10 0 |> should equal [1024]

[<Test>]
let ``degrees with n = 0 returns powers from 1`` () =
    degrees 0 3 |> should equal [1;2;4;8]
    degrees 0 5 |> should equal [1;2;4;8;16;32]
