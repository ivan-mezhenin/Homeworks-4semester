open NetworkSimulation
open System

let simulate () =
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
    net.Connect 2 3
    net.Connect 3 4
    net.Connect 4 5

    c1.Infect()

    printfn "=== Начальное состояние ==="
    net.PrintState()

    let mutable step = 0
    let maxSteps = 20

    while net.GetInfectedCount() < 5 && step < maxSteps do
        step <- step + 1
        printfn $"\n--- Ход {step} ---"
        net.Tick()
        net.PrintState()

simulate ()