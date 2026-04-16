module PhoneDirectoryTests

open NUnit.Framework
open FsUnit
open PhoneDirectory.Core
open PhoneDirectory.Domain

[<Test>]
let ``PhoneBook.add adds contact correctly`` () =
    let book = PhoneBook.empty
    let book2 = PhoneBook.add "Иван" "+79991234567" book

    PhoneBook.findPhone "Иван" book2 |> should equal (Some "+79991234567")
    PhoneBook.findPhone "Петр" book2 |> should equal None

[<Test>]
let ``PhoneBook.findName finds by phone`` () =
    let book = 
        PhoneBook.empty
        |> PhoneBook.add "Анна" "+79161234567"
        |> PhoneBook.add "Мария" "+79261234567"

    PhoneBook.findName "+79161234567" book |> should equal (Some "Анна")

[<Test>]
let ``update Add adds contact and returns message`` () =
    let state = initialState
    let newState, messages = update (Add ("Тест", "+71234567890")) state

    PhoneBook.findPhone "Тест" newState.Data |> should equal (Some "+71234567890")
    messages |> should be Empty
    newState.LastMessage |> should equal (Some "Добавлен: Тест → +71234567890")

[<Test>]
let ``update Add with empty name or phone returns error`` () =
    let state = initialState
    let newState, messages = update (Add ("", "+7999")) state

    newState.Data |> should equal state.Data
    messages |> should equal ["Ошибка: имя и телефон не могут быть пустыми"]

[<Test>]
let ``update FindPhone returns correct message`` () =
    let book = PhoneBook.add "Иван" "+79991234567" PhoneBook.empty
    let state = { Data = book; LastMessage = None }

    let _, messages = update (FindPhone "Иван") state
    messages |> should equal ["Иван: +79991234567"]

[<Test>]
let ``update FindPhone when not found`` () =
    let _, messages = update (FindPhone "Неизвестный") initialState
    messages |> should equal ["Контакт 'Неизвестный' не найден"]

[<Test>]
let ``update ListAll shows all contacts`` () =
    let book = 
        PhoneBook.empty
        |> PhoneBook.add "Алексей" "+79161112233"
        |> PhoneBook.add "Борис" "+79162223344"

    let state = { Data = book; LastMessage = None }
    let _, messages = update ListAll state

    messages |> should contain "Содержимое справочника:"
    messages |> should contain "Алексей              +79161112233"
    messages |> should contain "Борис                +79162223344"

[<Test>]
let ``update Save and Load work`` () =
    let tempFile = System.IO.Path.GetTempFileName()
    
    let state = { Data = PhoneBook.add "Тест" "+79998887766" PhoneBook.empty; LastMessage = None }
    
    let newState, _ = update (Save tempFile) state
    
    System.IO.File.Exists(tempFile) |> should be True
    
    let loadedState, _ = update (Load tempFile) initialState
    
    PhoneBook.findPhone "Тест" loadedState.Data |> should equal (Some "+79998887766")

    System.IO.File.Delete(tempFile)
