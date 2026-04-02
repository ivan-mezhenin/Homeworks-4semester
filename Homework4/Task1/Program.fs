open PhoneBook.Core
open System

let rec readLine prompt =
    Console.Write prompt
    let input = Console.ReadLine().Trim()
    if String.IsNullOrWhiteSpace input then
        printfn "Ввод не может быть пустым"
        readLine prompt
    else input

let rec readCommand () : Command option =
    printfn "\nКоманды:"
    printfn "  add     — добавить контакт"
    printfn "  find    — найти телефон по имени"
    printfn "  who     — найти имя по телефону"
    printfn "  list    — показать весь справочник"
    printfn "  save    — сохранить в файл"
    printfn "  load    — загрузить из файла"
    printfn "  exit    — выйти"
    printfn ""

    match readLine "> " |> _.ToLower() with
    | "exit" | "q" | "quit" -> None

    | "add" ->
        let name = readLine "Имя: "
        let phone = readLine "Телефон: "
        Some (Add (name, phone))

    | "find" ->
        let name = readLine "Имя для поиска: "
        Some (FindPhone name)

    | "who" ->
        let phone = readLine "Телефон для поиска: "
        Some (FindName phone)

    | "list" ->
        Some ListAll

    | "save" ->
        let path = readLine "Путь к файлу для сохранения: "
        Some (Save path)

    | "load" ->
        let path = readLine "Путь к файлу для загрузки: "
        Some (Load path)

    | _ ->
        printfn "Неизвестная команда"
        readCommand ()

[<EntryPoint>]
let main _ =
    let rec loop state =
        let newState, messages = update (readCommand () |> Option.defaultValue Exit) state

        messages |> List.iter (printfn "%s")
        newState.LastMessage |> Option.iter (printfn "%s")

        match newState.LastMessage with
        | Some m when m.Contains("До свидания") -> 0
        | _ -> loop newState

    loop initialState