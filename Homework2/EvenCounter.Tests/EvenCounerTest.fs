module EvenCountTest

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.Xunit
open EvenCount

[<Property(Verbose = true, MaxTest = 10000)>]
let ``All three even count functions are equivalent`` (lst: int list) =
    let r1 = countEvensFilter lst
    let r2 = countEvensMapFold lst
    let r3 = countEvensFold lst

    r1 = r2 && r2 = r3

[<Test>]
let ``countEvensFilter works correctly`` () =
    countEvensFilter [1; 2; 3; 4; 5; 6] |> should equal 3
    countEvensFilter [1; 3; 5; 7] |> should equal 0
    countEvensFilter [2; 4; 6; 8] |> should equal 4
    countEvensFilter [] |> should equal 0

[<Test>]
let ``countEvensMapFold works correctly`` () =
    countEvensMapFold [10; 15; 20; 25; 30] |> should equal 3
    countEvensMapFold [-2; -1; 0; 1; 2] |> should equal 3

[<Test>]
let ``countEvensFold works correctly`` () =
    countEvensFold [0; 1; 2; 3; 4; 5; 6; 7; 8] |> should equal 5

[<Test>]
let ``all functions return 0 on empty list`` () =
    countEvensFilter [] |> should equal 0
    countEvensMapFold [] |> should equal 0
    countEvensFold [] |> should equal 0

[<Test>]
let ``functions work on large list`` () =
    let largeList = [1 .. 10000]
    let expected = largeList |> List.filter (fun x -> x % 2 = 0) |> List.length
    
    countEvensFilter largeList |> should equal expected
    countEvensMapFold largeList |> should equal expected
    countEvensFold largeList |> should equal expected
