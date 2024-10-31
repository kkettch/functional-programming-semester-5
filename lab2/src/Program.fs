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
    let tree = insertMany [10; 15; 3; 8; 20] tree

    printTree tree 0

    0
