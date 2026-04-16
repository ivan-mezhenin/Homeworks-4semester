namespace NetworkSimulation
open System

type Network() =
    let mutable computers: Map<int, Computer> = Map.empty
    let mutable random = Random.Shared

    member this.AddComputer(comp: Computer) =
        computers <- Map.add comp.Id comp computers

    member this.Connect id1 id2 =
        match Map.tryFind id1 computers, Map.tryFind id2 computers with
        | Some c1, Some c2 ->
            c1.Neighbors <- id2 :: c1.Neighbors
            c2.Neighbors <- id1 :: c2.Neighbors
        | _ -> ()

    member this.GetComputer id = Map.tryFind id computers

    member this.Tick() =
        let infectedNow = 
            computers.Values 
            |> Seq.filter (fun c -> c.IsInfected)
            |> Seq.toList

        let candidates = 
            infectedNow
            |> List.collect (fun c -> c.Neighbors)
            |> List.distinct
            |> List.choose (fun id -> this.GetComputer id)
            |> List.filter (fun c -> not c.IsInfected)

        for candidate in candidates do
            let prob = candidate.ShouldBeInfected()
            if random.NextDouble() < prob then
                candidate.Infect()

    member this.PrintState() =
        printfn "\nСостояние сети:"
        for comp in computers.Values |> Seq.sortBy (fun c -> c.Id) do
            let status = if comp.IsInfected then "ЗАРАЖЁН" else "чистый"
            printfn "Компьютер %d [%A] — %s" comp.Id comp.OS status

    member this.GetInfectedCount() =
        computers.Values |> Seq.filter (fun c -> c.IsInfected) |> Seq.length

    member this.SetRandomSeed seed =
        random <- Random(seed)