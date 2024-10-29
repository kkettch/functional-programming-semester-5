namespace RedBlackTreeTests

open Xunit
open FsCheck
open RbBag 

module Tests =
    
    [<Fact>]
    let ``Insert creates a valid red-black tree`` () =
        let tree = empty |> add 10 |> add 10 |> add 5
        match tree with
        | Node (Black, 10, Node (Red, 5, Empty, Empty), Node (Red, 10, Empty, Empty)) -> ()
        | _ -> failwith $"Tree structure is invalid. Expected:Node (Black, 10, Node (Red, 5, Empty, Empty), Node (Red, 10, Empty, Empty)), but got: {tree}"

    [<Fact>]
    let ``Delete reduces the tree size`` () =
        let tree = empty |> add 10 |> add 20 |> add 5
        let newTree = delete 10 tree
        match newTree with
        | Node(Black, 20, Node(Red, 5, Empty, Empty), Empty) -> ()
        | _ -> failwith $"Delete operation did not produce expected tree. Expected: Node(Black, 20, Node(Black, 5, Empty, Empty), Empty), but got: {newTree}"

    [<Fact>]
    let ``Filter retains nodes satisfying the predicate`` () =
        let tree = empty |> add 10 |> add 20 |> add 5
        let filteredTree = filter (fun x -> x > 10) tree
        match filteredTree with
        | Node(_, 20, Empty, Empty) -> ()
        | _ -> failwith $"Filter operation did not produce expected tree. Expected: Node(_, 20, Empty, Empty), but got: {filteredTree}"

    [<Fact>]
    let ``Map applies function to all values`` () =
        let tree = empty |> add 10 |> add 20 |> add 5
        let mappedTree = map ((+) 1) tree
        match mappedTree with
        | Node (Black, 11, Node (Red, 6, Empty, Empty), Node (Red, 21, Empty, Empty)) -> ()
        | _ -> failwith $"Map operation did not produce expected tree. Expected: Node(_, 11, Node(_, 21, Empty, Empty), Node(_, 6, Empty, Empty)), but got: {mappedTree}"

    [<Fact>]
    let ``FoldLeft aggregates values correctly`` () =
        let tree = empty |> add 10 |> add 20 |> add 5
        let result = foldLeft (+) 0 tree
        Assert.Equal(35, result)
    
    [<Fact>]
    let ``foldRight aggregates values correctly`` () =
        let tree = empty |> add 10 |> add 20 |> add 6
        let result = foldLeft (+) 0 tree
        Assert.Equal(36, result)
        
    [<Fact>]
    let ``Combine is associative`` () =
        let treeA = empty |> add 10
        let treeB = empty |> add 20
        let treeC = empty |> add 30

        let combined1 = combine (combine treeA treeB) treeC
        let combined2 = combine treeA (combine treeB treeC)

        Assert.Equal(combined1, combined2)


