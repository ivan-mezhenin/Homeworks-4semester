module ParsingTree

type Expr =
    | Number of int
    | Negative of Expr
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

let rec evaluate (expr: Expr) : int =
    match expr with
    | Number n          -> n
    | Negative e          -> - (evaluate e)
    | Add (a, b)     -> evaluate a + evaluate b
    | Sub (a, b)     -> evaluate a - evaluate b
    | Mul (a, b)     -> evaluate a * evaluate b
    | Div (a, b)     ->
        let divisor = evaluate b
        if divisor = 0 then failwith "Division by zero"
        evaluate a / divisor