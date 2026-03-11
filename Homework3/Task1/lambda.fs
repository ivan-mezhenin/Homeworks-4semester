module lambda

type Lambda = 
    | Var of string
    | App of Lambda * Lambda
    | Abs of string * Lambda

type rec FV = function
    | Var x -> Set.singleton x
    | App (a,b) -> Set.union (FV x) (FV y) 
    | Abs (x, F) -> Set.remove x (FV F)

let rec newVarName usedNames currentVarName =
    let rec generateNewName n =
        let name = if n = 0 then currentVarName else sprintf "%s%d" basename n
        if Set.contains name usedNames then generateNewName (n + 1) else name
    generateNewName 0
