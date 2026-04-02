module NumberOfDegrees

let degrees n m =
    if m < 0 then []
    else
        let rec lstOfDegress current last lst acc =
            match current with
            | c when c > last -> List.rev lst
            | _ -> lstOfDegress (current + 1) last (acc :: lst) (acc * 2.0)

        lstOfDegress n (n + m) [] (2.0 ** float n)