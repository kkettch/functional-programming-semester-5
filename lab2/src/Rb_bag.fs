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

let empty = Empty

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

let combineTrees left right =
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
            combineTrees leftFiltered rightFiltered
