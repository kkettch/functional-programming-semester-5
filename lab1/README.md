# Лабораторная работа №1

`ФИО: Чмурова Мария Владиславовна` <br />
`Isu_id: 369027` <br />
`Группа: P3332` <br />

### Список задач 
---
**Задача №8** 
Найти максимально произведение последовательности из 13-подряд идущих цифр в числе из 1000 цифр. 

---
**Задача №21**
`d(n)` - сумма всех делителей числа n 
Необходимо найти сумму всех чисел, меньших 10000, для которых выполняются условия: 
- `d(a) = b` и `d(b) = a`
- `a != b`
 
---
### Задача №8

#### Проблема 

Найти максимально произведение последовательности из 13-подряд идущих цифр в числе из 1000 цифр. 

#### Решение 

1. Генерация последовательности при помощи отображения (map)

```F#
let stringToIntList str = 
    str
    |> Seq.map (fun digit -> bigint (int (string digit)))
    |> Seq.toList
```

2. Хвостовая рекурсия для нахождения максимального произведения (upd. pattern matching)

```F#
let rec findMaxTailRecursion nbrList maxProd i =
    match i, List.length nbrList with
    | _, len when i > len - 13 -> maxProd
    | _ ->
        let curNbr = List.take 13 (List.skip i nbrList)
        let prod = List.fold (*) 1I curNbr
        let newMaxProd = if prod > maxProd then prod else maxProd
        findMaxTailRecursion nbrList newMaxProd (i + 1)
```

3.  Рекурсия используя сопоставление с образцом (pattern matching)

```F#
let rec findMaxRecursion nbrList = 
    match nbrList with
    | [] -> 0I
    | _ when List.length nbrList < 13 -> 0I
    | _ -> 
        let prod = 
            List.take 13 nbrList 
            |> List.fold (*) 1I 
        max prod (findMaxRecursion (List.tail nbrList))
```

4. Модульная реализация и использование List.map. Отдельные функции для получения всех подпоследовательностей из 13 цифр и нахождения произведения

```F#
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
```

5. Реализация на Python:

```Python
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
```

--- 
### Задача №21

#### Проблема 

`d(n)` - сумма всех делителей числа n 
Необходимо найти сумму всех чисел, меньших 10000, для которых выполняются условия: 
- `d(a) = b` и `d(b) = a`
- `a != b`

#### Решение

1. Хвостовая рекурсия для реализации функции `d(n)` - нахождения суммы всех делителей числа `n` (upd. pattern matching)

```F#
let d n =
    let rec findSum acc i =
        match i with
        | i when i >= n -> acc
        | i when n % i = 0 -> findSum (acc + i) (i + 1)
        | _ -> findSum acc (i + 1)
    findSum 0 1
```

2. Рекурсия для нахождения дружественных чисел < 10000 (upd. pattern matching)

```F#
let rec getAmicableNumbersRecursion n limit acc =
    match n with
    | n when n >= limit -> acc
    | _ ->
        let b = d n
        match b with
        | b when b <> n && d b = n && b < limit -> getAmicableNumbersRecursion (n + 1) limit (n :: acc)
        | _ -> getAmicableNumbersRecursion (n + 1) limit acc
```

3. Нахождение дружественных чисел с использованием map:
- Находятся все дружественные пары (a, d(a), удовлетворяющие условию
- Считается сумма  всех первых элементов пар

```F#
let getAmicablePairs limit =
    [1 .. limit-1]
    |> List.map (fun a -> (a, d a))
    |> List.filter (fun (a, b) -> a <> b && d b = a && b < limit)

let getAmicableNumbersMap = 
    getAmicablePairs 10000
    |> List.map fst

let amicableSumMap = List.sum getAmicableNumbersMap
```

4. Ленивые коллекции и использование специального синтаксиса для циклов (upd. infinite seq)

```F#
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
```

5. Еще один вариант реализации, используя List.filter

```F#
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
```

6. Реализация на Python:

```Python
def d(n):
    summa = 1 
    for i in range(2, int(n**0.5) + 1):
        if n % i == 0:
            summa += i
            if i != n // i:  
                summa += n // i
    return summa
def get_amicable_numbers(limit):
    amicable_numbers = []
    for a in range(2, limit):
        b = d(a)
        if b != a and d(b) == a:
            amicable_numbers.append(a)
    return amicable_numbers
amicable_numbers = get_amicable_numbers(10000)
```

---
### Выводы

В ходе данной лабораторной работы я познакомилась с основами синтаксиса языка F#, написанием функций и некоторыми особенностями функционального языка, такими как неизменные переменные и ленивые вычисления. Данная работа была больше интересной, чем сложной, несмотря на начальные непонятки при освоении синтаксиса.  
Наибольшую сложность вызвало освоение, запоминание и использование всех используемых функций классов List и Seq для работы со списками и последовательностями. Интересно было поработать с pattern matching (сопоставление с образцом), он напоминает логику switch/case в C.
