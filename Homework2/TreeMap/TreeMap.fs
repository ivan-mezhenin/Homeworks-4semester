module TreeMap

type BinaryTree<'a> =
    | Empty
    | Node of 'a * BinaryTree<'a> * BinaryTree<'a>

let rec mapTree (f: 'a -> 'b) (tree: BinaryTree<'a>) : BinaryTree<'b> =
    match tree with
    | Empty -> Empty
    | Node (value, left, right) ->
        Node (f value, mapTree f left, mapTree f right)