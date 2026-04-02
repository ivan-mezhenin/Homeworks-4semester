module TreeMapTest

open NUnit.Framework
open FsUnit
open TreeMap

let leaf x = Node (x, Empty, Empty)

[<Test>]
let ``mapTree on single node tree`` () =
    let tree = leaf 5
    let result = mapTree (fun x -> x * 2) tree
    result |> should equal (leaf 10)

[<Test>]
let ``mapTree doubles all values in tree`` () =
    let tree = 
        Node (1, 
            Node (2, leaf 4, Empty),
            Node (3, Empty, leaf 5))

    let expected = 
        Node (2, 
            Node (4, leaf 8, Empty),
            Node (6, Empty, leaf 10))

    mapTree (fun x -> x * 2) tree |> should equal expected

[<Test>]
let ``mapTree works with string to int conversion`` () =
    let tree = 
        Node ("a", 
            leaf "bb", 
            Node ("ccc", Empty, leaf "dddd"))

    let expected = 
        Node (1, 
            leaf 2, 
            Node (3, Empty, leaf 4))

    mapTree String.length tree |> should equal expected

[<Test>]
let ``mapTree works with negative numbers`` () =
    let tree = Node (-1, leaf (-5), leaf 10)
    let result = mapTree (fun x -> x * (-1)) tree
    
    result |> should equal (Node (1, leaf 5, leaf (-10)))

[<Test>]
let ``mapTree preserves tree structure`` () =
    let tree = 
        Node (10, 
            Node (20, Empty, Empty),
            Node (30, 
                Node (40, Empty, Empty),
                Empty))

    let result = mapTree (fun x -> x + 1) tree

    match result with
    | Node (11, Node (21, Empty, Empty), Node (31, Node (41, Empty, Empty), Empty)) -> ()
    | _ -> Assert.Fail "Tree structure was not preserved"

[<Test>]
let ``mapTree with identity function returns same tree`` () =
    let tree = 
        Node (1, 
            Node (2, leaf 3, Empty),
            leaf 4)

    mapTree id tree |> should equal tree

[<Test>]
let ``mapTree works with bool to string`` () =
    let tree = Node (true, leaf false, leaf true)
    let result = mapTree (fun b -> if b then "Yes" else "No") tree

    result |> should equal 
        (Node ("Yes", leaf "No", leaf "Yes"))