module NumberOfDegrees

let degrees n m = 
    let rec lstOfDegress current last lst acc =
        match current with
        |_ when current = last -> (acc :: lst) |> List.rev
        |_ -> lstOfDegress (current + 1) last (acc :: lst) (acc * 2) 

    lstOfDegress n (n + m) [] (pown 2 n)