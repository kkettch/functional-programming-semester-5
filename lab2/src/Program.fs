module Main

open RbBag

[<EntryPoint>]
let main argv =
    let rec printTree (tree: 'a Tree) (level: int) =
        match tree with
        | Empty -> ()
        | Node { value = v; color = c; left = l; right = r } ->
            printTree r (level + 1)
            printfn "%s%s %A" (String.replicate (level * 4) " ") (if c = Red then "R" else "B") v
            printTree l (level + 1)

    let tree = empty
    let tree = insert 10 tree
    let tree = insert 10 tree
    let tree = insert 15 tree
    let tree = insert 3 tree
    let tree = insert 8 tree
    let tree = insert 20 tree

    // Выводим дерево после вставок
    printTree tree 0

    0