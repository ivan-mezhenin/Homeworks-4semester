module Search

let search lst n = 
    let rec loop lst acc =
        match lst with 
        | h :: _ when h = n -> Some acc 
        | _ :: t -> loop t (acc + 1) 
        | [] -> None
    
    loop lst 0