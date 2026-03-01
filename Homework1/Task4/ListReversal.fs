module ListReversal


let reverse lst = List.fold (fun acc x -> x :: acc) [] lst