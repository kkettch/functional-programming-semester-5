module MaxProduct

open System.Numerics

let stringToIntList str = 
    str
    |> Seq.map (fun digit -> bigint (int (string digit)))
    |> Seq.toList

let rec findMaxTailRecursion nbrList maxProd i = 
    if i > (List.length nbrList - 13) then maxProd
    else
        let curNbr = 
            List.skip i nbrList
            |> List.take 13
        let prod: BigInteger = List.fold (*) 1I curNbr
        let newMaxProd = 
            if prod > maxProd then prod else maxProd
        findMaxTailRecursion nbrList newMaxProd (i + 1)

let rec maxProdTailRecursion str =
    let nbrList = stringToIntList str
    findMaxTailRecursion nbrList 0I 0

let rec findMaxRecursion nbrList = 
    match nbrList with
    | [] -> 0I
    | _ when List.length nbrList < 13 -> 0I
    | _ -> 
        let prod = 
            List.take 13 nbrList 
            |> List.fold (*) 1I 
        max prod (findMaxRecursion (List.tail nbrList))

let rec maxProdRecursion str =
    let nbrList = stringToIntList str
    findMaxRecursion nbrList

let getSubSeq nbrList = 
    nbrList
    |> List.windowed 13

let getProd nbrList =
    List.fold (*) 1I nbrList

let maxProdModule str = 
    str
    |> stringToIntList
    |> getSubSeq
    |> List.map getProd
    |> List.max

