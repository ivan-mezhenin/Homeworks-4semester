namespace Homeworks6

type CalculateBuilder() =
    member _.Bind(value: string, f: float -> float option) : float option =
        match System.Double.TryParse(value) with
        | true, v -> f v
        | _ -> None

    member _.Return(value: float) : float option = Some value

    member _.ReturnFrom(value: float option) = value

module Calculate =
    let calculate = CalculateBuilder()
