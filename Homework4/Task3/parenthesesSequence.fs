module parenthesesSequence

let hasCorrectParentheses (s: string) : bool =
    let pairs = Map.ofList [ '(', ')'; '[', ']'; '{', '}' ]

    let chars = Seq.toList s

    let rec loop stack remaining =
        match remaining with
        | [] -> 
            List.isEmpty stack

        | c :: rest when Map.containsKey c pairs -> 
            loop (c :: stack) rest

        | c :: rest when pairs |> Map.exists (fun _ v -> v = c) -> 
            match stack with
            | top :: tail when pairs[top] = c -> 
                loop tail rest
            | _ -> false

        | _ :: rest -> 
            loop stack rest

    loop [] chars