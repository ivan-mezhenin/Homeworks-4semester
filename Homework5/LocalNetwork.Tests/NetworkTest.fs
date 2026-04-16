module NetworkTests

open NUnit.Framework
open FsUnit
open NetworkSimulation

type MockRandom(fixedProbability: float) =
    interface IRandom with
        member _.NextDouble() = fixedProbability


[<Test>]
let ``Virus with probability 1.0 spreads like BFS`` () =
    let net = Network()

    let c1 = Computer(1, Windows)
    let c2 = Computer(2, Linux)
    let c3 = Computer(3, Windows)
    let c4 = Computer(4, MacOS)
    let c5 = Computer(5, Linux)

    net.AddComputer c1
    net.AddComputer c2
    net.AddComputer c3
    net.AddComputer c4
    net.AddComputer c5

    net.Connect 1 2
    net.Connect 1 3
    net.Connect 2 4
    net.Connect 3 5

    c1.Infect()

    let mock = MockRandom(1.0)
    [c1; c2; c3; c4; c5] |> List.iter (fun c -> c.SetRandomProvider mock)

    net.Tick()
    net.Tick()

    net.GetInfectedCount() |> should equal 5

    c2.IsInfected |> should be True
    c3.IsInfected |> should be True
    c4.IsInfected |> should be True
    c5.IsInfected |> should be True


[<Test>]
let ``Virus with probability 0.0 infects only initial computer`` () =
    let net = Network()

    let c1 = Computer(1, Windows)
    let c2 = Computer(2, Linux)
    let c3 = Computer(3, MacOS)

    net.AddComputer c1
    net.AddComputer c2
    net.AddComputer c3

    net.Connect 1 2
    net.Connect 2 3

    c1.Infect()

    let mock = MockRandom(0.0)
    [c1; c2; c3] |> List.iter (fun c -> c.SetRandomProvider mock)

    net.Tick()
    net.Tick()

    c1.IsInfected |> should be True
    c2.IsInfected |> should be False
    c3.IsInfected |> should be False


[<Test>]
let ``Partial probability infects only some neighbors`` () =
    let net = Network()

    let c1 = Computer(1, Windows)
    let c2 = Computer(2, Linux)
    let c3 = Computer(3, MacOS)

    net.AddComputer c1
    net.AddComputer c2
    net.AddComputer c3

    net.Connect 1 2
    net.Connect 2 3

    c1.Infect()

    c2.SetRandomProvider (MockRandom(1.0))
    c3.SetRandomProvider (MockRandom(0.0))

    net.Tick()

    c2.IsInfected |> should be True
    c3.IsInfected |> should be False


[<Test>]
let ``Multiple steps with mixed probabilities`` () =
    let net = Network()

    let c1 = Computer(1, Windows)
    let c2 = Computer(2, Linux)
    let c3 = Computer(3, Windows)
    let c4 = Computer(4, MacOS)

    net.AddComputer c1
    net.AddComputer c2
    net.AddComputer c3
    net.AddComputer c4

    net.Connect 1 2
    net.Connect 2 3
    net.Connect 3 4

    c1.Infect()

    c2.SetRandomProvider (MockRandom(1.0))
    c3.SetRandomProvider (MockRandom(0.5))
    c4.SetRandomProvider (MockRandom(0.0))

    net.Tick()
    net.Tick()

    c2.IsInfected |> should be True
    c4.IsInfected |> should be False


[<Test>]
let ``Clear infection state works between tests`` () =
    let net = Network()
    let c1 = Computer(1, Windows)
    net.AddComputer c1

    c1.Infect()
    c1.IsInfected |> should be True

    c1.IsInfected <- false
    c1.IsInfected |> should be False