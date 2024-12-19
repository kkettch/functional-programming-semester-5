module RbBag

type Color = 
    | Red
    | Black

type 'a Tree = 
    | Empty
    | Node of 'a TreeNode
and 'a TreeNode = {
    value : 'a; 
    color : Color; 
    left : 'a Tree; 
    right : 'a Tree;
}

let empty : 'a Tree = Empty

let rec isMember tree value =
    match tree with
    | Empty -> false
    | Node {value = treeValue; color = _; left = left; right = right} ->
        if value < treeValue then isMember left value
        else if value > treeValue then isMember right value
        else true

let balance color x a b =
    match (color, x, a, b) with
    | (Black, z, // LL imbalance
        Node {
            value = y; 
            color = Red; 
            left = Node {value = x; color = Red; left = a; right = b}; 
            right = c
        }, d)     
    | (Black, z, // LR imbalance
        Node {
            value = x; 
            color = Red; 
            left = a; 
            right = Node {value = y; color = Red; left = b; right = c}; 
        }, d)    
    | (Black, x, a, // RL imbalance
        Node { 
            value = z; 
            color = Red; 
            left = Node {value = y; color = Red; left = b; right = c}; 
            right = d; 
        })    
    | (Black, x, a, // RR imbalance
        Node { 
            value = y; 
            color = Red; 
            left = b; 
            right = Node {value = z; color = Red; left = c; right = d}; 
        }) ->
        Node {  
            value = y; 
            color = Red; 
            left = Node {value = x; color = Black; left = a; right = b}; 
            right = Node {value = z; color = Black; left = c; right = d}
        }
    | _ -> Node {value = x; color = color; left = a; right = b}

let insert x tree =
    
    let makeBlack = 
        function
        | Node {value = y; color = _; left = left; right = right} -> Node {value = y; color = Black; left = left; right = right}
        | Empty -> failwith "Unexpected case: tree is empty"

    let rec insertOnCondition tree =
        match tree with
        | Empty -> Node {value = x; color = Red; left = Empty; right = Empty}
        | Node {value = y; color = color; left = a; right = b} ->
            if x < y then balance color y (insertOnCondition a) b
            else balance color y a (insertOnCondition b)

    makeBlack (insertOnCondition tree)

let insertMany xs tree =
    let switch f = fun y x -> f x y
    xs 
    |> Seq.fold (switch insert) tree

let rec delete x tree=

    let rec findMin tree =
        match tree with
        | Node {color = _; value = value; left = Empty; right = _} -> value
        | Node {color = _; value = _; left = left; right = _} -> findMin left
        | Empty -> failwith "Unexpected case: tree is empty"

    let rec deleteMin tree =
        match tree with
        | Node {value = _; color = _; left = Empty; right = right} -> right
        | Node {value = value; color = color; left = left; right = right} ->
            balance color value (deleteMin left) right
        | Empty -> failwith "Unexpected case: tree is empty"

    let rec deleteNode tree =
        match tree with
        | Node {value = value; color = color; left = left; right = right} ->
            if x < value then balance color value (deleteNode left) right
            else if x > value then balance color value left (deleteNode right)
            else match right with
                 | Empty -> left
                 | _ -> let minValue = findMin right
                        balance color minValue left (deleteMin right)
        | Empty -> Empty

    match tree with
    | Empty -> Empty
    | Node _ -> deleteNode tree

let rec map mapFunction tree =
    match tree with
    | Empty -> Empty
    | Node {value = value; color = color; left = left; right = right} ->
        Node { 
            value = mapFunction value; 
            color = color; 
            left = map mapFunction left; 
            right = map mapFunction right 
        }

let rec foldLeft f acc tree =
    match tree with
    | Empty -> acc
    | Node {value = value; color = _; left = left; right = right} ->
        let accLeftSubtree = foldLeft f acc left
        let accCounted = f accLeftSubtree value
        foldLeft f accCounted right

let rec foldRight f tree acc =
    match tree with
    | Empty -> acc
    | Node {value = value; color = _; left = left; right = right} ->
        let accRightSubtree = foldRight f right acc
        let accCounted = f accRightSubtree value 
        foldRight f left accCounted

let combine left right =
    match left, right with
    | Empty, _ -> right
    | _, Empty -> left
    | Node {value = value; color = color; left = _; right = _} as node, _ ->
        Node {value = value; color = color; left = left; right = right}

let rec filter pred tree =
    match tree with
    | Empty -> Empty
    | Node {value = value; color = color; left = left; right = right} ->
        let leftFiltered = filter pred left
        let rightFiltered = filter pred right
        if pred value then
            Node {value = value; color = color; left = leftFiltered; right = rightFiltered}
        else
            combine leftFiltered rightFiltered

let rec insertNode value tree =
    match tree with
    | Empty -> Node { value = value; color = Red; left = Empty; right = Empty }
    | Node { value = v; color = color; left = left; right = right } ->
        if value < v then balance color v (insertNode value left) right
        else balance color v left (insertNode value right)

let rec merge tree1 tree2 =
    match tree1, tree2 with
    | Empty, _ -> tree2
    | _, Empty -> tree1
    | Node { value = value; color = color; left = left; right = right }, _ ->
        let mergedLeft = merge left tree2
        let mergedTree = insertNode value mergedLeft
        merge right mergedTree

let rec equal tree1 tree2 =
    match tree1 with
    | Empty -> true
    | Node {value = v; left = l; right = r} ->
        isMember tree2 v && equal l tree2 && equal r tree2

let rec allElementsSatisfy tree pred =
    match tree with
    | Empty -> true
    | Node {value = v; left = l; right = r} ->
        pred v && allElementsSatisfy l pred && allElementsSatisfy r pred

(*
    Проверка корректности покраски - корень черный
*)
let rootIsBlack tree =
    match tree with
    | Empty -> true
    | Node { color = Black; value = _; left = _; right = _; } -> true
    | _ -> false

(*
    Проверка корректности покраски - нет двух красных узлов подряд
*)
let rec noRRImbalance tree =
    match tree with
    | Empty -> true
    | Node { color = Black; left = l; right = r } ->
        noRRImbalance l && noRRImbalance r
    | Node { color = Red; left = l; right = r } ->
        match l, r with
        | Node { color = Red; value = _; left = l; right = r; }, _ -> false
        | _, Node { color = Red; value = _; left = l; right = r; } -> false
        | _ -> noRRImbalance l && noRRImbalance r
        
(*
    Проверка корректности покраски - одинаковое количество черных узлов на каждом пути
*)
let rec blackDeepth tree =
    match tree with
    | Empty -> 1
    | Node { color = Black; value = _; left = l; right = _ } -> 1 + blackDeepth l
    | Node { color = Red; value = _; left = l; right = _ } -> blackDeepth l

let rec sameBlackDeepth tree height =
    match tree with
    | Empty -> height = 1
    | Node { color = Black; left = l; right = r } ->
        sameBlackDeepth l (height - 1) &&
        sameBlackDeepth r (height - 1)
    | Node { color = Red; left = l; right = r } ->
        sameBlackDeepth l height &&
        sameBlackDeepth r height