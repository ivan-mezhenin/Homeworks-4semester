module NumberOfDegreesTests

open NUnit.Framework
open FsUnit
open NumberOfDegrees

let shouldEqualFloat (expected: float) (actual: float) =
    actual |> should (equalWithin 1e-10) expected

let shouldEqualFloatList (expected: float list) (actual: float list) =
    actual |> should haveLength expected.Length
    List.zip expected actual
    |> List.iter (fun (exp, act) -> shouldEqualFloat exp act)

[<TestCase(0, 0, [|1.0|])>]
[<TestCase(1, 0, [|2.0|])>]
[<TestCase(2, 0, [|4.0|])>]
[<TestCase(3, 0, [|8.0|])>]
[<TestCase(0, 1, [|1.0; 2.0|])>]
[<TestCase(0, 2, [|1.0; 2.0; 4.0|])>]
[<TestCase(0, 3, [|1.0; 2.0; 4.0; 8.0|])>]
[<TestCase(1, 1, [|2.0; 4.0|])>]
[<TestCase(1, 2, [|2.0; 4.0; 8.0|])>]
[<TestCase(1, 3, [|2.0; 4.0; 8.0; 16.0|])>]
let ``degrees should return correct powers of two`` (n, m, expected: float[]) =
    let result = degrees n m
    shouldEqualFloatList (List.ofArray expected) result

[<Test>]
let ``degrees with negative n works correctly`` () =
    degrees (-3) 2 |> shouldEqualFloatList [0.125; 0.25; 0.5]
    degrees (-1) 3 |> shouldEqualFloatList [0.5; 1.0; 2.0; 4.0]
    degrees (-2) 0 |> shouldEqualFloatList [0.25]

[<Test>]
let ``degrees with m = 0 returns single element`` () =
    degrees 5 0  |> shouldEqualFloatList [32.0]
    degrees (-4) 0 |> shouldEqualFloatList [0.0625]

[<Test>]
let ``degrees returns empty list when m is negative`` () =
    degrees 5 (-1) |> should be Empty
    degrees (-3) (-2) |> should be Empty
    degrees 0 (-5) |> should be Empty
    degrees 10 (-1) |> should be Empty

[<Test>]
let ``degrees with n = 0 returns correct sequence`` () =
    degrees 0 4 |> shouldEqualFloatList [1.0; 2.0; 4.0; 8.0; 16.0]

[<Test>]
let ``degrees handles large positive n without overflow`` () =
    degrees 20 5 |> shouldEqualFloatList 
        [1048576.0; 2097152.0; 4194304.0; 8388608.0; 16777216.0; 33554432.0]