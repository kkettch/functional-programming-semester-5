module Main

open RbBag

let rec printTree = function
    | Empty -> "Empty"
    | Node(color, value, left, right) ->
        let colorStr = 
            match color with
            | Red -> "Red"
            | Black -> "Black"
        sprintf "%s(%A) [L: %s, R: %s]" colorStr value (printTree left) (printTree right)

[<EntryPoint>]
let main argv =
    let tree = empty
    let tree = 
        tree 
        |> add "banana" 
        |> add "apple" 
        |> add "cherry" 
        |> add "date" 
        |> add "fig"
    
    printfn "добавление элементов banana, apple, cherry, date, fig:\n%s" (printTree tree)

    let tree = delete "cherry" tree
    printfn "удаление cherry:\n%s" (printTree tree)

    let filteredTree = filter (fun x -> x > "banana") tree
    printfn "все значения > banana:\n%s" (printTree filteredTree)

    let mappedTree = map (fun x -> x + " fruit") tree
    printfn "добавить 'fruit' к каждому значению:\n%s" (printTree mappedTree)

    // Так как сумма строк не имеет смысла, мы можем вывести все значения в дереве
    let stringValues = foldLeft (fun acc x -> acc + ", " + x) "" tree
    printfn "все значения: %s" stringValues

    let anotherTree = add "kiwi" (add "lemon" Empty)
    let combinedTree = combine tree anotherTree
    printfn "объединенные деревья:\n%s" (printTree combinedTree)

    0