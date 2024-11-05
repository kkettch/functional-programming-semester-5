module RbBagTests

open FsCheck.Xunit
open Xunit
open RbBag

let rec allElementsSatisfy tree pred =
    match tree with
    | Empty -> true
    | Node {value = v; left = l; right = r} ->
        pred v && allElementsSatisfy l pred && allElementsSatisfy r pred

let rec allElementsInOtherTree tree otherTree =
    match tree with
    | Empty -> true
    | Node {value = v; left = l; right = r} ->
        isMember otherTree v && allElementsInOtherTree l otherTree && allElementsInOtherTree r otherTree

[<Property>]
let ``Empty tree is neutral element for insertMany`` (xs: int list) =
    let tree = insertMany xs empty
    let combinedWithEmptyLeft = insertMany [] tree
    let combinedWithEmptyRight = insertMany xs empty
    combinedWithEmptyLeft = tree && combinedWithEmptyRight = tree

[<Property>]
let ``Tree combination is associative`` (xs: int list) (ys: int list) (zs: int list) =
    let tree1 = insertMany xs empty
    let tree2 = insertMany ys empty
    let tree3 = insertMany zs empty
    let combined1 = insertMany ys (insertMany xs tree3)
    let combined2 = insertMany xs (insertMany ys tree3)
    allElementsInOtherTree combined1 combined2 && allElementsInOtherTree combined2 combined1

[<Property>]
let ``Map preserves elements in tree`` (xs: int list) =
    let tree = insertMany xs empty
    let mappedTree = map ((+) 1) tree
    let rec checkMapped tree =
        match tree with
        | Empty -> true
        | Node {value = v; left = l; right = r} ->
            isMember mappedTree (v + 1) && checkMapped l && checkMapped r
    checkMapped tree

[<Property>]
let ``InsertMany preserves elements`` (xs: int list) =
    let t = insertMany xs empty
    let rec checkElementsInTree tree elements =
        match elements with
        | [] -> true
        | x::xs -> isMember tree x && checkElementsInTree tree xs
    checkElementsInTree t xs

[<Property>]
let ``Filter should only retain elements satisfying predicate`` (xs: int list) =
    let tree = insertMany xs empty
    let pred x = x > 3
    let filteredTree = filter pred tree
    allElementsSatisfy filteredTree pred

