module EvenCount

let countEvensFilter lst =
    lst
    |> List.filter (fun x -> x % 2 = 0)
    |> List.length

let countEvensMapFold lst =
    lst
    |> List.map (fun x -> if x % 2 = 0 then 1 else 0)
    |> List.fold (+) 0

let countEvensFold lst =
    lst
    |> List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0