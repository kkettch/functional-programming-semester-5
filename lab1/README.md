# Лабораторная работа №1

`ФИО: Чмурова Мария Владиславовна` <br />
`Isu_id: 369027` <br />
`Группа: P3332` <br />

### Список задач 
---
**Задача №8** <br />
Найти максимально произведение последовательности из 13-подряд идущих цифр в числе из 1000 цифр. 

---
**Задача №21** <br />
`d(n)` - сумма всех делителей числа n 
Необходимо найти сумму всех чисел, меньших 10000, для которых выполняются условия: 
- `d(a) = b` и `d(b) = a`
- `a != b`
 
---
### Задача №8

#### Проблема 

Найти максимально произведение последовательности из 13-подряд идущих цифр в числе из 1000 цифр. 

#### Решение 

1. Генерация последовательности при помощи отображения (map). Используем для преобразование числа из типа string в list

```
let stringToIntList str =
    str 
    |> Seq.map (fun ch -> bigint (int (string ch)))
    |> Seq.toList
```

2. Хвостовая рекурсия для нахождения максимального произведения

```
let rec findUsingTailRecursion numbersList maxProd i =
    if i > (List.length numbersList - 13) then maxProd
    else 
        let currentDigits = 
            List.skip i numbersList 
            |> List.take 13
        let product = List.fold (*) 1I currentDigits
        let newMaxProd = if product > maxProd then product else maxProd
        findUsingTailRecursion numbersList newMaxProd (i + 1)
```

3. Способ нахождения максимального произведения при помощи рекурсии 

```
let rec findUsingRecursion numbersList =
    match numbersList with
    | [] -> 0I
    | _ when List.length numbersList < 13 -> 0I
    | _ ->
        let product = 
            List.take 13 numbersList 
            |> List.fold (*) 1I
        max product (findUsingRecursion (List.tail numbersList))
```

4. Модульная реализация с разбиением на отдельные функции и использование List.map. Использование отдельных функций для получения всех подпоследовательностей из 13 цифр и нахождения произведения. 

```
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
```

5. Реализация на Python:

```
strToInt = [int(digit) for digit in number]

def find_product(strToInt):
    product = 1
    for number in strToInt:
        product *= number
    return product

max_prod = 0

for i in range(len(strToInt) - 13 + 1):
    curent_sequence = strToInt[i:i+13]
    current_prod = find_product(curent_sequence)
    if current_prod > max_prod:
        max_prod = current_prod

print("Максимальное произведение: ", max_prod)
```

--- 
### Задача №21

#### Проблема 

`d(n)` - сумма всех делителей числа n 
Необходимо найти сумму всех чисел, меньших 10000, для которых выполняются условия: 
- `d(a) = b` и `d(b) = a`
- `a != b`

#### Решение

1. Хвостовая рекурсия использована для реализации функции `d(n)` - нахождения суммы всех делителей

```
let d1 n =
    let rec aux acc i =
        if i >= n then acc
        else if n % i = 0 then aux (acc + i) (i + 1)
        else aux acc (i + 1)
    aux 0 1
```

2. Рекурсия для нахождения всех "дружественных" пар чисел 

```
let rec findAmicableNumbers n limit acc =
    if n >= limit then acc
    else
        let b = d1 n
        if b < limit && b <> n && d1 b = n then
            findAmicableNumbers (n + 1) limit (n :: acc)
        else
            findAmicableNumbers (n + 1) limit acc
```

3. Генерация пар числа и значения функции при помощи map. Выбирается только одно число из каждой пары, так как для двух чисел необходимо выбрать только само число, а не результат его функции:  

```
let amicablePairs limit =
    [1 .. limit-1]
    |> List.map (fun a -> (a, d2 a))
    |> List.filter (fun (a, b) -> b < limit && b <> a && d2 b = a)

let amicableNumbers = amicablePairs 10000 |> List.map fst
let amicableSumMap = List.sum amicableNumbers
```

4. Использование специального синтаксиса для цикла for: 

```
let findAmicableNumbersLoop limit =
    let amicableNumbers = ref []
    for n in 1 .. limit - 1 do
        let b = d2 n
        if b < limit && b <> n && d2 b = n then
            amicableNumbers := n :: !amicableNumbers
    List.sum !amicableNumbers
```

5. Ленивые коллекции: 

```
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
```

6. Реализация на Python:

```

```

---
### Выводы

В ходе данной лабораторной работы 
