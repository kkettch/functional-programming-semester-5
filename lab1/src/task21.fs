module AmicableNumbers

// 1.1. Хвостовая рекурсия для нахождения суммы делителей 

let d1 n =
    let rec aux acc i =
        if i >= n then acc
        else if n % i = 0 then aux (acc + i) (i + 1)
        else aux acc (i + 1)
    aux 0 1


// 1.2. Рекурсия для нахождения "дружественных" чисел меньше 10000

let rec findAmicableNumbers n limit acc =
    if n >= limit then acc
    else
        let b = d1 n
        if b < limit && b <> n && d1 b = n then
            findAmicableNumbers (n + 1) limit (n :: acc)
        else
            findAmicableNumbers (n + 1) limit acc

let amicableSumRec = List.sum (findAmicableNumbers 1 10000 [])

// 2. Модульная реализация

let properDivisors n = 
    [1 .. n/2] 
    |> List.filter (fun x -> n % x = 0)

let d2 n = List.sum (properDivisors n)

let findAmicableNumbersModular limit =
    [1 .. limit-1]
    |> List.filter (fun a ->
        let b = d2 a
        b < limit && b <> a && d2 b = a
    )

let amicableSumModular = List.sum (findAmicableNumbersModular 10000)

// 3. Генерация пар при помощи map

let amicablePairs limit =
    [1 .. limit-1]
    |> List.map (fun a -> (a, d2 a))
    |> List.filter (fun (a, b) -> b < limit && b <> a && d2 b = a)

let amicableNumbers = amicablePairs 10000 |> List.map fst
let amicableSumMap = List.sum amicableNumbers

// 4. Специальный синтаксис для циклов

let findAmicableNumbersLoop limit =
    let amicableNumbers = ref []
    for n in 1 .. limit - 1 do
        let b = d2 n
        if b < limit && b <> n && d2 b = n then
            amicableNumbers := n :: !amicableNumbers
    List.sum !amicableNumbers

let amicableSumLoop = findAmicableNumbersLoop 10000

// 5. Ленивые коллекции 

let amicableNumbersUnder limit =
    seq {
        for n in 1 .. limit - 1 do
            let b = d1 n
            if b < limit && b <> n && d1 b = n then
                yield n
    }

let amicableSumLazy = 
    amicableNumbersUnder 10000 
    |> Seq.sum
