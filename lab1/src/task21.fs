module AmicableNumbers

(*
    upd. pattern matching
*)
let d n =
    let rec findSum acc i =
        match i with
        | i when i >= n -> acc
        | i when n % i = 0 -> findSum (acc + i) (i + 1)
        | _ -> findSum acc (i + 1)
    findSum 0 1

(*
    upd. pattern matching
*)
let rec getAmicableNumbersRecursion n limit acc =
    match n with
    | n when n >= limit -> acc
    | _ ->
        let b = d n
        match b with
        | b when b <> n && d b = n && b < limit -> getAmicableNumbersRecursion (n + 1) limit (n :: acc)
        | _ -> getAmicableNumbersRecursion (n + 1) limit acc

let amicableSumRecursion = List.sum (getAmicableNumbersRecursion 1 10000 [])

let getAmicablePairs limit =
    [1 .. limit-1]
    |> List.map (fun a -> (a, d a))
    |> List.filter (fun (a, b) -> a <> b && d b = a && b < limit)

let getAmicableNumbersMap = 
    getAmicablePairs 10000
    |> List.map fst

let amicableSumMap = List.sum getAmicableNumbersMap

(*
    upd. infinite seq.
*)
let infiniteSeqAmicableNumbers () =
    Seq.initInfinite (fun n -> n + 1) 
    |> Seq.filter (fun n -> 
        let b = d n
        b <> n && d b = n 
    )

let getAmicableNumbersLazy limit =
    infiniteSeqAmicableNumbers ()
    |> Seq.takeWhile (fun n -> n < limit) 

let amicableSumLazy = Seq.sum (getAmicableNumbersLazy 10000)


let sumDivisors n =
    [1 .. n/2]
    |> List.filter (fun x -> n % x = 0)
    |> List.sum

let getAmicableNumberFilter limit =
    [1 .. limit - 1]
    |> List.filter 
        (fun n -> 
            let b = sumDivisors n
            b <> n && sumDivisors b = n && b < limit
        )

let amicableSumFilter = List.sum (getAmicableNumberFilter 10000)


