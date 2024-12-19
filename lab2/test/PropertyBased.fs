module CustomTreeGen

open FsCheck.Xunit
open FsCheck
open RbBag

open FsCheck

type ArbitraryTree<'T when 'T : comparison>() =
    static member Tree =
        Arb.fromGen <| 
            gen {
                let! size = Gen.choose (10, 50)
                let! values = Gen.listOfLength size (Arb.generate<'T>)
                return List.fold (fun acc n -> insert n acc) Empty values
            }

[<Property(Arbitrary = [| typeof<ArbitraryTree<int>> |])>]
let prop_emptyMerge (bag: Tree<int>) =
    equal bag (merge bag Empty)

[<Property(Arbitrary = [| typeof<ArbitraryTree<int>> |])>]
let prop_associativity (bag1: Tree<int>) (bag2: Tree<int>) (bag3: Tree<int>) =
    let mergedBag = merge bag1 (merge bag2 bag3)
    let mergedBag2 = merge (merge bag1 bag2) bag3

    equal mergedBag mergedBag2


[<Property(Arbitrary = [| typeof<ArbitraryTree<int>> |])>]
let ``Empty tree is neutral element for merge`` (tree: Tree<int>) =
    let combinedWithEmptyLeft = merge empty tree
    let combinedWithEmptyRight = merge tree empty

    combinedWithEmptyLeft = tree && combinedWithEmptyRight = tree

[<Property(Arbitrary = [| typeof<ArbitraryTree<int>> |])>]
let ``Map preserves elements in tree`` (tree: Tree<int>) =
    let mappedTree = map ((+) 1) tree
    let rec checkMapped tree =
        match tree with
        | Empty -> true
        | Node {value = v; left = l; right = r} ->
            isMember mappedTree (v + 1) && checkMapped l && checkMapped r
    checkMapped tree


[<Property(Arbitrary = [| typeof<ArbitraryTree<int>> |])>]
let ``Filter should only retain elements satisfying predicate`` (tree: Tree<int>) =
    let pred x = x > 3
    let filteredTree = filter pred tree

    allElementsSatisfy filteredTree pred


(*
    Покраска - дерево удовлетворяет всем правилам
*)
[<Property(Arbitrary = [| typeof<ArbitraryTree<int>> |])>]
let ``Tree coloring is correct`` (tree: Tree<int>) =
    let bh = blackDeepth tree

    rootIsBlack tree && 
    noRRImbalance tree && 
    sameBlackDeepth tree bh
