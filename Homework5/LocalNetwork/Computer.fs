namespace NetworkSimulation

open System

type IRandom =
    abstract member NextDouble : unit -> float

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

    member val private RandomProvider: IRandom = 
        { new IRandom with member _.NextDouble() = Random.Shared.NextDouble() } 
        with get, set

    member this.SetRandomProvider (provider: IRandom) =
        this.RandomProvider <- provider

    member this.ShouldBeInfected() : float =
        this.RandomProvider.NextDouble()

    member this.Infect() =
        this.IsInfected <- true