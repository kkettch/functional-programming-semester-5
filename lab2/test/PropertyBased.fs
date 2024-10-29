namespace RedBlackTreeTests

open RbBag
open FsCheck.Xunit

module PropertyTests =

    [<Property>]
    let ``Adding an element results in a tree containing that element`` (x: int) =
        let tree = empty |> add x
        match tree with
        | Node(_, value, _, _) -> value = x
        | Empty -> false

    [<Property>]
    let ``Adding an existing element does not change the tree structure`` (x: int) =
        let tree = empty |> add x
        let newTree = add x tree
        match newTree, tree with
        | Node(_, newValue, _, _), Node(_, oldValue, _, _) -> newValue = oldValue
        | _ -> false

    [<Property>]
    let ``foldLeft correctly sums all values in the tree`` (xs: int list) =
        let tree = List.fold (fun acc x -> add x acc) empty xs
        let result = foldLeft (+) 0 tree
        let expected = List.sum xs
        result = expected
    
    [<Property>]
    let ``Combining trees is associative`` (x: int) (y: int) (z: int) =
        let tree1 = add x empty
        let tree2 = add y empty
        let tree3 = add z empty
        let leftAssociative = 
            combine (combine tree1 tree2) tree3 = combine tree1 (combine tree2 tree3)
        leftAssociative
