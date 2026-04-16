open PhoneDirectory.Core
open System

let rec readLine (prompt : string) =
    Console.Write prompt
    let input = Console.ReadLine().Trim()
    if String.IsNullOrWhiteSpace input then
        printfn "Ввод не может быть пустым"
        readLine prompt
    else input

let rec readCommand () : Command =
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
    | "exit" | "q" | "quit" -> Exit
    | "add" ->
        let name = readLine "Имя: "
        let phone = readLine "Телефон: "
        Add (name, phone)
    | "find" ->
        let name = readLine "Имя для поиска: "
        FindPhone name
    | "who" ->
        let phone = readLine "Телефон для поиска: "
        FindName phone
    | "list" -> ListAll
    | "save" ->
        let path = readLine "Путь к файлу: "
        Save path
    | "load" ->
        let path = readLine "Путь к файлу: "
        Load path
    | _ ->
        printfn "Неизвестная команда"
        readCommand ()

let rec loop state =
    let newState, messages = update (readCommand ()) state

    messages |> List.iter (printfn "%s")
    newState.LastMessage |> Option.iter (printfn "%s")

    match newState.LastMessage with
    | Some m when m.Contains("До свидания") -> ()
    | _ -> loop newState


loop initialState