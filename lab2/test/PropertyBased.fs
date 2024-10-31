module RbBagTests

open FsCheck.Xunit
open Xunit
open RbBag

let combineTreesMonoid (t1: 'a Tree) (t2: 'a Tree) : 'a Tree =
    insertMany (foldLeft (fun acc x -> x :: acc) [] t1) t2

[<Property>]
let ``Combining two trees gives a valid tree`` (xs: int list) (ys: int list) =
    let tree1 = insertMany xs empty
    let tree2 = insertMany ys empty
    let combined = combineTreesMonoid tree1 tree2
    List.forall (fun x -> isMember combined x) (xs @ ys)

[<Property>]
let ``Combining two empty trees results in an empty tree`` () =
    let emptyTree1 = empty
    let emptyTree2 = empty
    let combinedEmptyTrees = combineTreesMonoid emptyTree1 emptyTree2
    combinedEmptyTrees = Empty

[<Property>]
let ``Filter should only retain elements satisfying predicate`` (xs: int list) =
    let tree = insertMany xs empty
    let pred x = x > 0
    let filteredTree = filter pred tree
    let allRetained = List.forall (fun x -> pred x = (isMember filteredTree x)) xs
    allRetained

[<Property>]
let ``Map preserves elements in tree`` (xs: int list) =
    let tree = insertMany xs empty
    let mappedTree = map ((+) 1) tree
    let allMapped = List.forall (fun x -> isMember mappedTree (x + 1)) xs
    allMapped

[<Property>]
let ``All elements in int list are members after insertion`` (xs: int list) =
    let tree = insertMany xs empty
    xs |> List.forall (fun x -> isMember tree x)

[<Property>]
let ``Empty tree is neutral element for insertMany`` (xs: int list) =
    let t = insertMany xs empty
    Assert.Equal(t, insertMany [] t)

[<Property>]
let ``InsertMany preserves elements`` (xs: int list) =
    let t = insertMany xs empty
    for x in xs do
        Assert.True(isMember t x)
