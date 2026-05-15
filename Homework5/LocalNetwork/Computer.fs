namespace NetworkSimulation

open System

type IRandom =
    abstract member NextDouble : unit -> float

type IOS =
    abstract InfectionProbability: float

type Windows() =
    interface IOS with
        member _.InfectionProbability = 0.85

type Linux() =
    interface IOS with
        member _.InfectionProbability = 0.30

type MacOS() =
    interface IOS with
        member _.InfectionProbability = 0.15

type Computer(id: int, os: IOS, infected: bool) =
    member val Id = id
    member val OS = os
    member val IsInfected = infected with get, set
    new(id: int, os: IOS) = Computer(id, os, false)

    member val private RandomProvider: IRandom =
        let rng = Random()
        { new IRandom with member _.NextDouble() = rng.NextDouble() }
        with get, set

    member this.SetRandomProvider (provider: IRandom) =
        this.RandomProvider <- provider

    member this.TryInfect() : bool =
        if this.RandomProvider.NextDouble() < this.OS.InfectionProbability then
            this.IsInfected <- true
            true
        else
            false

    member this.Infect() =
        this.IsInfected <- true
