module AmicableNumbers

let d n =
    let rec findSum acc i =
        if i >= n then acc
        elif n % i = 0 then findSum (acc + i) (i + 1)
        else findSum acc (i + 1)
    findSum 0 1

let rec getAmicableNumbersRecursion n limit acc =
    if n >= limit then acc
    else
        let b = d n
        if b <> n && d b = n && b < limit then
            getAmicableNumbersRecursion (n + 1) limit (n :: acc)
        else getAmicableNumbersRecursion (n + 1) limit acc

let amicableSumRecursion = List.sum (getAmicableNumbersRecursion 1 10000 [])

let getAmicablePairs limit =
    [1 .. limit-1]
    |> List.map (fun a -> (a, d a))
    |> List.filter (fun (a, b) -> a <> b && d b = a && b < limit)

let getAmicableNumbersMap = 
    getAmicablePairs 10000
    |> List.map fst

let amicableSumMap = List.sum getAmicableNumbersMap

let getAmicableNumbersLazy limit =
    seq {
        for n in 1 .. limit-1 do
            let b = d n
            if b <> n && d b = n && b < limit then
                yield n
    }

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


