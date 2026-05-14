namespace Homeworks6

type RoundingBuilder(precision: int) =
    member _.Bind(value: float, f: float -> float) : float =
        let rounded = System.Math.Round(value, precision)
        f rounded

    member _.Return(value: float) : float =
        System.Math.Round(value, precision)

module Rounding =
    let rounding precision = RoundingBuilder(precision)
