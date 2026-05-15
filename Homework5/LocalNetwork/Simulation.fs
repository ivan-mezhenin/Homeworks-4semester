namespace NetworkSimulation

module Simulation =
    let run (net: Network) (maxSteps: int) =
        let mutable step = 0
        while net.CanStateChange() && step < maxSteps do
            step <- step + 1
            printfn $"\n--- Ход {step} ---"
            net.Tick()
            net.PrintState()
