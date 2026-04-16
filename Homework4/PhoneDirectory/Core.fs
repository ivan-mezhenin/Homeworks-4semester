module PhoneDirectory.Core

open PhoneDirectory.Domain
open System.Text.Json

type Command =
    | Exit
    | Add of name: string * phone: string
    | FindPhone of name: string
    | FindName of phone: string
    | ListAll
    | Save of path: string
    | Load of path: string

type AppState = {
    Data: PhoneBook
    LastMessage: string option
}

let initialState = { Data = PhoneBook.empty; LastMessage = None }

let update (cmd: Command) (state: AppState) : AppState * string list =
    match cmd with
    | Exit ->
        { state with LastMessage = Some "До свидания!" }, []

    | Add (name, phone) ->
        if System.String.IsNullOrWhiteSpace name || System.String.IsNullOrWhiteSpace phone then
            state, ["Ошибка: имя и телефон не могут быть пустыми"]
        else
            let newData = PhoneBook.add name phone state.Data
            { state with Data = newData; LastMessage = Some $"Добавлен: {name} → {phone}" }, []

    | FindPhone name ->
        match PhoneBook.findPhone name state.Data with
        | Some phone -> state, [$"{name}: {phone}"]
        | None -> state, [$"Контакт '{name}' не найден"]

    | FindName phone ->
        match PhoneBook.findName phone state.Data with
        | Some name -> state, [$"{phone} принадлежит: {name}"]
        | None -> state, [$"Владелец номера {phone} не найден"]

    | ListAll ->
        let contacts = PhoneBook.toList state.Data
        if contacts.IsEmpty then
            state, ["Справочник пуст"]
        else
            let lines = contacts |> List.map (fun (n, p) -> sprintf "%-20s %s" n p)
            state, "Содержимое справочника:" :: lines

    | Save path ->
        try
            let json = JsonSerializer.Serialize(state.Data)
            System.IO.File.WriteAllText(path, json)
            { state with LastMessage = Some $"Сохранено в {path}" }, []
        with e ->
            state, [$"Ошибка сохранения: {e.Message}"]

    | Load path ->
        try
            let json = System.IO.File.ReadAllText(path)
            let data = JsonSerializer.Deserialize<PhoneBook>(json)
            { state with Data = data; LastMessage = Some $"Загружено из {path}" }, []
        with e ->
            state, [$"Ошибка загрузки: {e.Message}"]