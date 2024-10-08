module MaxProduct

open System.Numerics

let stringToIntList str =
    str 
    |> Seq.map (fun ch -> bigint (int (string ch)))
    |> Seq.toList

let rec findUsingTailRecursion numbersList maxProd i =
    if i > (List.length numbersList - 13) then maxProd
    else 
        let currentDigits = 
            List.skip i numbersList 
            |> List.take 13
        let product = List.fold (*) 1I currentDigits
        let newMaxProd = if product > maxProd then product else maxProd
        findUsingTailRecursion numbersList newMaxProd (i + 1)

let findMaxProductTailRecursion str =
    let numbersList = stringToIntList str
    findUsingTailRecursion numbersList 0I 0 

let rec findUsingRecursion numbersList =
    match numbersList with
    | [] -> 0I
    | _ when List.length numbersList < 13 -> 0I
    | _ ->
        let product = 
            List.take 13 numbersList 
            |> List.fold (*) 1I
        max product (findUsingRecursion (List.tail numbersList))

let findMaxProductRecursion str =
    let numbersList = stringToIntList str
    findUsingRecursion numbersList

let getSubsequences numbersList = 
    numbersList 
    |> List.windowed 13 

let productOfDigits numbersList =
    List.fold (*) 1I numbersList

let findMaxProductModule str =
    str
    |> stringToIntList
    |> getSubsequences
    |> List.map productOfDigits
    |> List.max
