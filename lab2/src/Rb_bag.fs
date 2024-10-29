module RbBag

type Color = 
    | Red 
    | Black

type RedBlackTree<'T> = 
    | Empty 
    | Node of Color * 'T * RedBlackTree<'T> * RedBlackTree<'T> 

let blackNode value left right = Node(Black, value, left, right)

let isRed = 
    function
    | Node(Red, _, _, _) -> true
    | _ -> false

let balance = 
    function
    | Black, z, Node(Red, y, Node(Red, x, a, b), c), d
    | Black, z, Node(Red, x, a, Node(Red, y, b, c)), d
    | Black, x, a, Node(Red, z, Node(Red, y, b, c), d)
    | Black, x, a, Node(Red, y, b, Node(Red, z, c, d)) ->
        Node(Red, y, blackNode x a b, blackNode z c d)
    | color, value, left, right ->
        Node(color, value, left, right)

let rec insert x = 
    function
    | Empty -> Node(Red, x, Empty, Empty) 
    | Node(color, y, left, right) as node ->
        if x < y then balance (color, y, insert x left, right) 
        else balance (color, y, left, insert x right) 

let add x tree =
    match insert x tree with
    | Node(_, y, left, right) -> Node(Black, y, left, right) 
    | Empty -> Empty

let rec delete x = 
    function
    | Empty -> Empty 
    | Node(color, y, left, right) ->
        if x < y then Node(color, y, delete x left, right) 
        elif x > y then Node(color, y, left, delete x right) 
        else 
            match left, right with
            | Empty, _ -> right 
            | _, Empty -> left
            | _, _ -> 
                let rec minValueNode = function
                    | Node(_, v, Empty, _) -> v
                    | Node(_, _, l, _) -> minValueNode l
                    | Empty -> failwith "empty tree error"
                let minValue = minValueNode right
                Node(color, minValue, left, delete minValue right)

let rec filter predicate = 
    function
    | Empty -> Empty 
    | Node(color, value, left, right) ->
        let leftFiltered = filter predicate left
        let rightFiltered = filter predicate right
        if predicate value then 
            Node(color, value, leftFiltered, rightFiltered)
        else
            match leftFiltered, rightFiltered with
            | Empty, _ -> rightFiltered
            | _, Empty -> leftFiltered
            | _ -> Node(color, value, leftFiltered, rightFiltered)

let rec map f = 
    function
    | Empty -> Empty 
    | Node(color, value, left, right) ->
        Node(color, f value, map f left, map f right)

let rec foldLeft f acc = 
    function
    | Empty -> acc
    | Node(_, value, left, right) ->
        let leftAcc = foldLeft f acc left
        let rightAcc = foldLeft f (f leftAcc value) right
        rightAcc

let rec foldRight f tree acc =
    match tree with
    | Empty -> acc
    | Node(_, value, left, right) ->
        let rightAcc = foldRight f right acc
        foldRight f left (f value rightAcc)

let empty = Empty
let combine t1 t2 = 
    foldLeft (fun acc x -> add x acc) t2 t1

