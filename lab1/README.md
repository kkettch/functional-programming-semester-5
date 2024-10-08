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

1. Генерация последовательности при помощи отображения (map). Используем для преобразование числа из типа string в list<int>

```
let stringToIntList str =
	str
	|> Seq.map (string >> int)
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
        let product = List.fold (*) 1 currentDigits
        let newMaxProd = if product > maxProd then product else maxProd
        findUsingTailRecursion numbersList newMaxProd (i + 1)
```

3. Способ нахождения максимального произведения при помощи рекурсии 

```
let rec findUsingRecursion numbersList =
    match numbersList with
    | [] -> 0
    | _ when List.length numbersList < 13 -> 0
    | _ ->
        let product = 
            List.take 13 numbersList 
            |> List.fold (*) 1
        max product (findUsingRecursion (List.tail numbersList))
```

4. Модульная реализация с разбиением на отдельные функции и использование List.map. Использование отдельных функций для получения всех подпоследовательностей из 13 цифр и нахождения произведения. 

```
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
```
