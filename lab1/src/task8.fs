module MaxProduct

// 3. Преобразуем число в список цифр, используя генерацию последовательности при помощи отображения (map)

let stringToIntList str =
    str 
    |> Seq.map (string >> int) 
    |> Seq.toList

// 1.1. Нахождение решения с помощью хвостовой рекурсии

let rec findUsingTailRecursion numbersList maxProd i =
    if i > (List.length numbersList - 13) then maxProd
    else 
        let currentDigits = 
            List.skip i numbersList 
            |> List.take 13
        let product = List.fold (*) 1 currentDigits
        let newMaxProd = if product > maxProd then product else maxProd
        findUsingTailRecursion numbersList newMaxProd (i + 1)

let findMaxProductTailRecursion str =
    let numbersList = stringToIntList str
    findUsingTailRecursion numbersList 0 0 

// 1.1. Рекурсия

let rec findUsingRecursion numbersList =
    match numbersList with
    | [] -> 0
    | _ when List.length numbersList < 13 -> 0
    | _ ->
        let product = 
            List.take 13 numbersList 
            |> List.fold (*) 1
        max product (findUsingRecursion (List.tail numbersList))

let findMaxProductRecursion str =
    let numbersList = stringToIntList str
    findUsingRecursion numbersList

// 4. Модульная реализация с разделением на функции и использование List.map и других функций высших порядкой:

let getSubsequences numbersList = 
    numbersList 
    |> List.windowed 13 

let productOfDigits numbersList =
    List.fold (*) 1 numbersList

let findMaxProductModule str =
    str
    |> stringToIntList
    |> getSubsequences
    |> List.map productOfDigits
    |> List.max

