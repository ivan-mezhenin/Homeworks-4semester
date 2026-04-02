module LambdaTests

open NUnit.Framework
open FsUnit
open lambda 


[<Test>]
let ``FV переменной возвращает множество с этой переменной`` () =
    FV (Var "x") |> should equal (Set.singleton "x")

[<Test>]
let ``FV абстракции удаляет связанную переменную`` () =
    FV (Abs("x", Var "x")) |> should be Empty

[<Test>]
let ``FV абстракции сохраняет свободные переменные в теле`` () =
    FV (Abs("x", Var "y")) |> should equal (Set.singleton "y")

[<Test>]
let ``FV аппликации объединяет множества`` () =
    FV (App(Var "x", Var "y")) |> should equal (Set.ofList ["x"; "y"])

[<Test>]
let ``FV сложного выражения корректно вычисляет свободные`` () =
    let expr = Abs("x", App(Var "x", Var "y"))
    FV expr |> should equal (Set.singleton "y")

[<Test>]
let ``substitute заменяет свободную переменную в Var`` () =
    substitute "x" (Var "y") (Var "x") |> should equal (Var "y")

[<Test>]
let ``substitute не заменяет другую переменную`` () =
    substitute "x" (Var "y") (Var "z") |> should equal (Var "z")

[<Test>]
let ``substitute работает в аппликации`` () =
    let expr = App(Var "x", Var "z")
    let expected = App(Var "y", Var "z")
    substitute "x" (Var "y") expr |> should equal expected

[<Test>]
let ``substitute в абстракции без конфликта`` () =
    let expr = Abs("z", Var "x")
    let expected = Abs("z", Var "y")
    substitute "x" (Var "y") expr |> should equal expected

[<Test>]
let ``substitute не заходит в абстракцию с тем же именем параметра (тень)`` () =
    let expr = Abs("x", Var "x")
    substitute "x" (Var "y") expr |> should equal expr

[<Test>]
let ``substitute выполняет альфа-конверсию при конфликте имён`` () =
    let expr = Abs("y", Var "x")
    let replacement = Var "y"           
    let result = substitute "x" replacement expr

    match result with
    | Abs (name, body) ->
        name |> should not' (equal "y")
        body |> should equal (Var "y")
    | _ -> Assert.Fail("Результат не является абстракцией")

[<Test>]
let ``isNormalForm для переменной возвращает true`` () =
    isNormalForm (Var "x") |> should be True

[<Test>]
let ``isNormalForm для абстракции без редексов возвращает true`` () =
    isNormalForm (Abs("x", Var "x")) |> should be True

[<Test>]
let ``isNormalForm для простой аппликации без редексов возвращает true`` () =
    isNormalForm (App(Var "f", Var "x")) |> should be True

[<Test>]
let ``isNormalForm для редекса возвращает false`` () =
    isNormalForm (App(Abs("x", Var "x"), Var "y")) |> should be False

[<Test>]
let ``isNormalForm для выражения с редексом внутри возвращает false`` () =
    let expr = App( App(Abs("x", Var "x"), Var "y"), Var "z" )
    isNormalForm expr |> should be False

[<Test>]
let ``findRedex находит прямой редекс`` () =
    let expr = App(Abs("x", Var "x"), Var "y")
    let expected = Some ("x", Var "x", Var "y")
    findRedex expr |> should equal expected

[<Test>]
let ``findRedex возвращает None для переменной`` () =
    findRedex (Var "x") |> should equal None

[<Test>]
let ``findRedex находит редекс в теле абстракции`` () =
    let expr = Abs("x", App(Abs("y", Var "y"), Var "z"))
    let expected = Some ("y", Var "y", Var "z")
    findRedex expr |> should equal expected

[<Test>]
let ``findRedex находит левый редекс в аппликации`` () =
    let expr = App( App(Abs("x", Var "x"), Var "y"), Var "z")
    let expected = Some ("x", Var "x", Var "y")
    findRedex expr |> should equal expected

[<Test>]
let ``findRedex находит редекс в правой части, если в левой нет`` () =
    let expr = App( Var "x", App(Abs("y", Var "y"), Var "z") )
    let expected = Some ("y", Var "y", Var "z")
    findRedex expr |> should equal expected

[<Test>]
let ``reduceOneStep редуцирует прямой редекс`` () =
    let expr = App(Abs("x", Var "x"), Var "y")
    reduceOneStep expr |> should equal (Var "y")

[<Test>]
let ``reduceOneStep редуцирует редекс в левой части аппликации`` () =
    let expr = App( App(Abs("x", Var "x"), Var "y"), Var "z")
    let expected = App( Var "y", Var "z" )
    reduceOneStep expr |> should equal expected

[<Test>]
let ``reduceOneStep редуцирует редекс в правой части аппликации`` () =
    let expr = App( Var "x", App(Abs("y", Var "y"), Var "z") )
    let expected = App( Var "x", Var "z" )
    reduceOneStep expr |> should equal expected

[<Test>]
let ``reduceOneStep редуцирует редекс в теле абстракции`` () =
    let expr = Abs("x", App(Abs("y", Var "y"), Var "z"))
    let expected = Abs("x", Var "z")
    reduceOneStep expr |> should equal expected

[<Test>]
let ``reduceOneStep для нормальной формы возвращает то же выражение`` () =
    let expr = Var "x"
    reduceOneStep expr |> should equal expr

[<Test>]
let ``normalize редуцирует цепочку аппликаций`` () =
    // ((λx.x) (λy.y)) z  →  z
    let expr = App( App(Abs("x", Var "x"), Abs("y", Var "y")), Var "z")
    normalize expr |> should equal (Var "z")

[<Test>]
let ``normalize для примера (λx.λy.x y) (λz.z) даёт λy.y`` () =
    let expr = App( Abs("x", Abs("y", App(Var "x", Var "y"))), Abs("z", Var "z") )
    let expected = Abs("y", Var "y")
    normalize expr |> should equal expected

[<Test>]
let ``normalize выполняет альфа-конверсию при необходимости`` () =
    // (λx.λy.x) y  →  λy1.y
    let expr = App( Abs("x", Abs("y", Var "x")), Var "y")
    let result = normalize expr
    match result with
    | Abs (name, body) ->
        name |> should not' (equal "y")
        body |> should equal (Var "y")
    | _ -> Assert.Fail("Результат должен быть абстракцией")

[<Test>]
let ``normalize для выражения без редексов возвращает его же`` () =
    let expr = Abs("x", App(Var "x", Var "y"))
    normalize expr |> should equal expr