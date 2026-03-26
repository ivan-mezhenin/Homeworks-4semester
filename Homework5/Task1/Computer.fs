namespace NetworkSimulation

open System

type OS =
    | Windows
    | Linux
    | MacOS

    member this.InfectionProbability =
        match this with
        | Windows -> 0.85
        | Linux   -> 0.30
        | MacOS   -> 0.15

type Computer(id: int, os: OS) =
    member val Id = id with get
    member val OS = os with get
    member val IsInfected = false with get, set
    member val Neighbors: int list = [] with get, set 

    member val private RandomOverride: float option = None with get, set

    member this.SetInfectionProbabilityForTest p =
        this.RandomOverride <- Some p

    member this.ShouldBeInfected() =
        match this.RandomOverride with
        | Some p -> p
        | None   -> this.OS.InfectionProbability

    member this.Infect() =
        this.IsInfected <- true