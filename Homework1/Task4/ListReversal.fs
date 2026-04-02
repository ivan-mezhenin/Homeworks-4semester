module ListReversal

let reverse lst =
    let rec loop remaining acc =
        match remaining with
        | [] -> acc
        | head :: tail -> loop tail (head :: acc)

    loop lst []