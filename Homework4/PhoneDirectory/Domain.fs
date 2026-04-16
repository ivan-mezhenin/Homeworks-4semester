module PhoneDirectory.Domain

type Contact = {
    Name: string
    Phone: string
}

type PhoneBook = Map<string, string>

[<RequireQualifiedAccess>]
module PhoneBook =
    let empty: PhoneBook = Map.empty

    let add name phone (book: PhoneBook) : PhoneBook =
        Map.add name phone book

    let findPhone name (book: PhoneBook) =
        Map.tryFind name book

    let findName phone (book: PhoneBook) =
        book |> Map.tryFindKey (fun _ p -> p = phone)

    let toList (book: PhoneBook) =
        book |> Map.toList |> List.sortBy fst