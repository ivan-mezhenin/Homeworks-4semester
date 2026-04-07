module ParenthesesTests

open NUnit.Framework
open FsUnit
open parenthesesSequence

[<Test>]
let ``hasCorrectParentheses returns true for empty string`` () =
    hasCorrectParentheses "" |> should equal true

[<Test>]
let ``hasCorrectParentheses returns true for simple correct parentheses`` () =
    hasCorrectParentheses "()" |> should equal true
    hasCorrectParentheses "[]" |> should equal true
    hasCorrectParentheses "{}" |> should equal true

[<Test>]
let ``hasCorrectParentheses returns true for nested parentheses`` () =
    hasCorrectParentheses "(())" |> should equal true
    hasCorrectParentheses "[[]]" |> should equal true
    hasCorrectParentheses "{{}}" |> should equal true
    hasCorrectParentheses "([{}])" |> should equal true

[<Test>]
let ``hasCorrectParentheses returns true for complex correct sequence`` () =
    hasCorrectParentheses "({[()]})" |> should equal true
    hasCorrectParentheses "abc(def[ghi]{jkl})" |> should equal true
    hasCorrectParentheses "a(b)c[d]e{f}g" |> should equal true

[<Test>]
let ``hasCorrectParentheses returns false for incorrect sequences`` () =
    hasCorrectParentheses "(" |> should equal false
    hasCorrectParentheses ")" |> should equal false
    hasCorrectParentheses "(]" |> should equal false
    hasCorrectParentheses "([)]" |> should equal false
    hasCorrectParentheses "{[}]" |> should equal false
    hasCorrectParentheses "((())" |> should equal false
    hasCorrectParentheses "())" |> should equal false

[<Test>]
let ``hasCorrectParentheses ignores non-bracket characters`` () =
    hasCorrectParentheses "a(b)c" |> should equal true
    hasCorrectParentheses "hello(world)" |> should equal true
    hasCorrectParentheses "[a + b * (c - d)]" |> should equal true
    hasCorrectParentheses "x{y}z" |> should equal true

[<Test>]
let ``hasCorrectParentheses works with multiple bracket types mixed correctly`` () =
    hasCorrectParentheses "([{}()])" |> should equal true
    hasCorrectParentheses "{[()()]}" |> should equal true

[<Test>]
let ``hasCorrectParentheses returns false for wrong closing order`` () =
    hasCorrectParentheses "([)]" |> should equal false
    hasCorrectParentheses "{(})" |> should equal false
    hasCorrectParentheses "[(])" |> should equal false

[<Test>]
let ``hasCorrectParentheses handles long correct sequence`` () =
    let longCorrect = "((()[]){}())"
    hasCorrectParentheses longCorrect |> should equal true