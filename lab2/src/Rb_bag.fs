module RbBag

type Color = Red | Black

type 'a Tree = 
    | Empty
    | Node of 'a TreeNode
and 'a TreeNode = { value : 'a; color : Color; left : 'a Tree; right : 'a Tree }

let empty : 'a Tree = Empty

let rec isMember (t : 'a Tree) (v : 'a) : bool =
    match t with
    | Empty -> false
    | Node { value = v'; color = _; left = l; right = r } ->
        if v < v' then isMember l v
        else if v > v' then isMember r v
        else true

let insert (x : 'a) (t : 'a Tree) : 'a Tree =
    let makeBlack = function
        | Node { value = y; color = _; left = a; right = b} -> Node { value = y; color = Black; left = a; right = b }
        | Empty -> failwith "Unexpected case: tree is empty"

    let rec balance (color : Color) (a : 'a Tree) (x : 'a) (b : 'a Tree) =
        match (color, a, x, b) with
        | (Black, Node { value = y; color = Red; left = Node { value = x; color = Red; left = a; right = b }; right = c}, z, d)
        | (Black, Node { value = x; color = Red; left = a; right = Node { value = y; color = Red; left = b; right = c }; }, z, d)
        | (Black, a, x, Node { value = z; color = Red; left = Node { value = y; color = Red; left = b; right = c }; right = d; })
        | (Black, a, x, Node { value = y; color = Red; left = b; right = Node { value = z; color = Red; left = c; right = d }; }) ->
            Node {  value = y; color = Red; 
                    left = Node {value = x; color = Black; left = a; right = b}; 
                    right = Node {value = z; color = Black; left = c; right = d}
                    }
        | _ -> Node { value = x; color = color; left = a; right = b }

    let rec ins t =
        match t with
        | Empty -> Node { value = x; color = Red; left = Empty; right = Empty }
        | Node { value = y; color = color; left = a; right = b } ->
            if x < y then balance color (ins a) y b
            else if x >= y then balance color a y (ins b)
            else Node { value = y; color = color; left = a; right = b }

    makeBlack (ins t)

let insertMany (xs : 'a seq) (t : 'a Tree) : 'a Tree =
    let switch f = fun y x -> f x y
    xs |> Seq.fold (switch insert) t

let rec delete (x : 'a) (t : 'a Tree) : 'a Tree =
    let rec balance (color : Color) (a : 'a Tree) (x : 'a) (b : 'a Tree) =
        match color, a, x, b with
        | (Black, Node { value = y; color = Red; left = Node { value = x; color = Red; left = a; right = b }; right = c }, z, d)
        | (Black, Node { value = x; color = Red; left = a; right = Node { value = y; color = Red; left = b; right = c } }, z, d)
        | (Black, a, x, Node { value = z; color = Red; left = Node { value = y; color = Red; left = b; right = c }; right = d })
        | (Black, a, x, Node { value = y; color = Red; left = b; right = Node { value = z; color = Red; left = c; right = d } }) ->
            Node { value = y; color = Red; left = Node { value = x; color = Black; left = a; right = b }; right = Node { value = z; color = Black; left = c; right = d } }
        | _ -> Node { value = x; color = color; left = a; right = b }

    let rec findMin t =
        match t with
        | Empty -> failwith "Unexpected case: tree is empty"
        | Node { value = v; left = Empty; right = _ } -> v
        | Node { left = l } -> findMin l

    let rec deleteMin t =
        match t with
        | Empty -> failwith "Unexpected case: tree is empty"
        | Node { value = v; color = c; left = Empty; right = r } -> r
        | Node { value = v; color = c; left = l; right = r } ->
            balance c (deleteMin l) v r

    let rec deleteNode t =
        match t with
        | Empty -> Empty
        | Node { value = v; color = c; left = l; right = r } ->
            if x < v then balance c (deleteNode l) v r
            else if x > v then balance c l v (deleteNode r)
            else match r with
                 | Empty -> l
                 | _ -> let m = findMin r
                        balance c l m (deleteMin r)

    match t with
    | Empty -> Empty
    | Node _ -> deleteNode t

let rec map (f : 'a -> 'b) (t : 'a Tree) : 'b Tree =
    match t with
    | Empty -> Empty
    | Node { value = v; color = c; left = l; right = r } ->
        Node { value = f v; color = c; left = map f l; right = map f r }

let rec foldLeft (f : 'b -> 'a -> 'b) (acc : 'b) (t : 'a Tree) : 'b =
    match t with
    | Empty -> acc
    | Node { value = v; color = _; left = l; right = r } ->
        let acc' = foldLeft f acc l
        let acc'' = f acc' v
        foldLeft f acc'' r

let rec foldRight (f : 'a -> 'b -> 'b) (t : 'a Tree) (acc : 'b) : 'b =
    match t with
    | Empty -> acc
    | Node { value = v; color = _; left = l; right = r } ->
        let acc' = foldRight f r acc
        let acc'' = f v acc'
        foldRight f l acc''

let rec filter (pred: 'a -> bool) (t: 'a Tree) : 'a Tree =
    match t with
    | Empty -> Empty
    | Node { value = v; color = c; left = l; right = r } ->
        let leftFiltered = filter pred l
        let rightFiltered = filter pred r
        if pred v then
            Node { value = v; color = c; left = leftFiltered; right = rightFiltered }
        else
            combineTrees leftFiltered rightFiltered

and combineTrees (l: 'a Tree) (r: 'a Tree) : 'a Tree =
    match l, r with
    | Empty, _ -> r
    | _, Empty -> l
    | Node { value = v; color = c; left = _; right = _ } as node, _ ->
        Node { value = v; color = c; left = l; right = r }

let rec contains (t: 'a Tree) (v: 'a) : bool =
    match t with
    | Empty -> false
    | Node { value = v'; color = _; left = l; right = r } ->
        if v < v' then contains l v
        else if v > v' then contains r v
        else true