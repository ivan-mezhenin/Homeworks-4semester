module lambda

type lambda = 
    | Var of string
    | App of lambda * lambda
    | Abs of string * lambda

let rec FV = function
    | Var x -> Set.singleton x
    | App (a,b) -> Set.union (FV a) (FV b) 
    | Abs (x, f) -> Set.remove x (FV f)

let rec newVarName usedNames currentVarName =
    let rec generateNewName n =
        let name = if n = 0 then currentVarName else sprintf "%s%d" currentVarName n
        if Set.contains name usedNames then generateNewName (n + 1) else name
    generateNewName 0

let rec substitute variableToReplace expressionToInsert targetExpression =
    let rec substInner BV currentExpr =
        match currentExpr with
        | Var y when y = variableToReplace -> 
            if Set.contains y BV then 
                failwith "Should not happen due to alpha conversion"
            else expressionToInsert
        | Var y -> Var y
        | App (e1, e2) -> App (substInner BV e1, substInner BV e2)
        | Abs (x, body) ->
            if x = variableToReplace then
                Abs (x, body)
            else
                let freeInWith = FV expressionToInsert
                if Set.contains x freeInWith then
                    let allVars = Set.union (FV currentExpr) (FV expressionToInsert)
                    let newName = newVarName allVars x
                    let newBody = substInner (Set.add newName BV) (substitute x (Var newName) body)
                    Abs (newName, substInner BV newBody)
                else
                    Abs (x, substInner (BV) body)
    substInner Set.empty targetExpression

let rec isNormalForm = function
    | Var _ -> true
    | Abs (_, body) -> isNormalForm body
    | App (Abs _, _) -> false
    | App (e1, e2) -> isNormalForm e1 && isNormalForm e2

let rec findRedex = function
    | App (Abs (x, body), arg) -> Some (x, body, arg)
    | App (e1, e2) ->
        match findRedex e1 with
        | Some redex -> Some redex
        | None -> findRedex e2
    | Abs (_, body) -> findRedex body
    | Var _ -> None

let fst3 (x, _, _) = x
let snd3 (_, y, _) = y
let trd3 (_, _, z) = z

let rec reduceOneStep = function
    | App (Abs (x, body), arg) ->
        substitute x arg body
    | App (e1, e2) ->
        match findRedex e1 with
        | Some redex -> 
            App (reduceOneStep (App (Abs (fst3 redex, snd3 redex), trd3 redex)), e2)
        | None ->
            App (e1, reduceOneStep e2)
    | Abs (x, body) ->
        match findRedex body with
        | Some redex -> Abs (x, reduceOneStep (App (Abs (fst3 redex, snd3 redex), trd3 redex)))
        | None -> Abs (x, body)
    | Var _ as expr -> expr

let rec normalize expr =
    if isNormalForm expr then 
        expr
    else 
        normalize (reduceOneStep expr)

let rec toString = function
    | Var x -> x
    | App (e1, e2) -> sprintf "(%s %s)" (toString e1) (toString e2)
    | Abs (x, e) -> sprintf "(λ%s.%s)" x (toString e)