namespace NetworkSimulation
open System

type Network(?comps: (int * IOS) list, ?conns: (int * int) list) as this =
    let mutable computers: Map<int, Computer> = Map.empty
    let mutable adjacency: Map<int, int list> = Map.empty

    do
        match comps with
        | Some c -> for (id, os) in c do this.AddComputer(Computer(id, os))
        | None -> ()
        match conns with
        | Some c -> for (id1, id2) in c do this.Connect id1 id2
        | None -> ()

    member this.AddComputer(comp: Computer) =
        computers <- Map.add comp.Id comp computers

    member this.Connect id1 id2 =
        match Map.tryFind id1 computers, Map.tryFind id2 computers with
        | Some _, Some _ ->
            adjacency <- adjacency
                         |> Map.change id1 (fun v -> Some (id2 :: Option.defaultValue [] v))
                         |> Map.change id2 (fun v -> Some (id1 :: Option.defaultValue [] v))
        | _ -> ()

    member this.GetComputer id = Map.tryFind id computers

    member this.Tick() =
        let infectedNow = 
            computers.Values 
            |> Seq.filter (fun c -> c.IsInfected)
            |> Seq.toList

        let candidates = 
            infectedNow
            |> List.collect (fun c -> Map.tryFind c.Id adjacency |> Option.defaultValue [])
            |> List.distinct
            |> List.choose (fun id -> this.GetComputer id)
            |> List.filter (fun c -> not c.IsInfected)

        for candidate in candidates do
            candidate.TryInfect() |> ignore

    member this.PrintState() =
        printfn "\nСостояние сети:"
        for comp in computers.Values |> Seq.sortBy (fun c -> c.Id) do
            let status = if comp.IsInfected then "ЗАРАЖЁН" else "чистый"
            printfn "Компьютер %d [%A] — %s" comp.Id comp.OS status

    member this.GetInfectedCount() =
        computers.Values |> Seq.filter (fun c -> c.IsInfected) |> Seq.length

    member this.CanStateChange() : bool =
        computers.Values
        |> Seq.exists (fun c ->
            if c.IsInfected then
                Map.tryFind c.Id adjacency
                |> Option.defaultValue []
                |> List.exists (fun nid ->
                    Map.tryFind nid computers
                    |> Option.exists (fun n -> not n.IsInfected)
                )
            else
                false
        )
