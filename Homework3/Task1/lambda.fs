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

