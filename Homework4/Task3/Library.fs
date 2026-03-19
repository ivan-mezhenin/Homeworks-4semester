module Library

let isBalanced (s: string) : bool =
    let pairs = Map.ofList [ '(', ')'; '[', ']'; '{', '}' ]

    let rec loop (stack: char list) (i: int) =
        if i >= s.Length then
            List.isEmpty stack
        else
            match s[i] with
            | c when Map.containsKey c pairs ->
                loop (c :: stack) (i + 1)
            | c when Map.containsValue c pairs ->
                match stack with
                | [] -> false
                | top :: rest when pairs[top] = c -> loop rest (i + 1)
                | _ -> false
            | _ -> loop stack (i + 1)

    loop [] 0