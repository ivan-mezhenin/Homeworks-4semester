module Square

/// Returns a list of strings representing a square of side n.
let squareLines n =
    match n with
    |n when n <= 0 -> []
    |n when n = 1 -> ["*"]                                 
    | _ ->
        let border = String.replicate n "*"
        let inner  = "*" + String.replicate (n - 2) " " + "*"
        
        border :: List.replicate (n - 2) inner @ [border]


/// Prints the square to the console.
let printSquare n =
    squareLines n
    |> List.iter (printfn "%s")