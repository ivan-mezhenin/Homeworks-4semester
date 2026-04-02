module MinElement

/// Finds the smallest element in a list.
/// Returns None if the list is empty.
let minElement lst =
    match lst with
    | [] -> None
    | _ -> Some (List.reduce min lst)
