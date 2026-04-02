namespace HashTableImplementation

open System

/// Simple hash table implementation.
type HashTable<'T when 'T : equality>(capacity: int, hashFunc: 'T -> int) =

    let mutable tables: 'T list array = Array.create capacity []

    let mutable count = 0

    /// Calculates the correct index for an element.
    let getIndex elem : int =
        let index = (hashFunc elem) % capacity
        if index < 0 then index + capacity
        else index

    /// Checks if the element exists in the hash table.
    member this.Contains elem : bool =
        let index = getIndex elem
        List.contains elem tables.[index]

    /// Adds an element to the hash table.
    member this.Add elem =
        let index = getIndex elem
        let bucket = tables.[index]

        if not (List.contains elem bucket) then
            tables.[index] <- elem :: bucket
            count <- count + 1

    /// Removes an element from the hash table if it exists.
    /// Returns true if the element was removed, false otherwise.
    member this.Remove elem : bool =
        let index = getIndex elem
        let bucket = tables.[index]

        if List.contains elem bucket then
            tables.[index] <- List.filter (fun x -> x <> elem) bucket
            count <- count - 1
            true
        else
            false

    /// Returns the current number of elements in the hash table.
    member this.Count = count

    /// Clears the hash table, removing all elements.
    member this.Clear() =
        tables <- Array.create capacity []
        count <- 0