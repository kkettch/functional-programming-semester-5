## Лабораторная работа №2

`ФИО: Чмурова Мария Владиславовна` <br />
`Isu_id: 369027` <br />
`Группа: P3332` <br />

## Задание
Реализовать структуру данных rb-bag

## Структура данных: 
```F#
type Color =
	| Red
	| Black

type RedBlackTree<'T> =
	| Empty
	| Node of Color * 'T * RedBlackTree<'T> * RedBlackTree<'T>
```
 
## Реализованные функции

1. Балансировка дерева для поддержания сбалансированности дерева: 
```F#
let rec balance color x a b =
        match (color, x, a, b) with
        | (Black, z, 
            Node {
                value = y; 
                color = Red; 
                left = Node 
	                { 
	                    value = x; 
	                    color = Red; 
	                    left = a; 
	                    right = b 
		            }; 
                right = c
			}, d)     // LL imbalance
        | (Black, z, 
	        Node 
	        {
		        value = x; 
		        color = Red; 
		        left = a; 
		        right = Node { 
			        value = y; 
			        color = Red; 
			        left = b; 
			        right = c }; 
			}, d)    // LR imbalance
        | (Black, x, a, Node { value = z; color = Red; left = Node { value = y; color = Red; left = b; right = c }; right = d; })    // RL imbalance
        | (Black, x, a, Node { value = y; color = Red; left = b; right = Node { value = z; color = Red; left = c; right = d }; }) ->    // RR imbalance
            Node {  value = y; color = Red; 
                    left = Node {value = x; color = Black; left = a; right = b}; 
                    right = Node {value = z; color = Black; left = c; right = d}
                    }
        | _ -> Node { value = x; color = color; left = a; right = b }
```

2. Вставка элемента в дерева. Элемент корня всегда черный (функц. add)
```F#
let rec insert x = 
    function
    | Empty -> Node(Red, x, Empty, Empty) 
    | Node(color, y, left, right) as node ->
        if x < y then balance (color, y, insert x left, right) 
        else balance (color, y, left, insert x right) 

let add x tree =
    match insert x tree with
    | Node(_, y, left, right) -> Node(Black, y, left, right) 
    | Empty -> Empty
```

3. Удаление элемента из дерева:
```F#
let rec delete x = 
    function
    | Empty -> Empty 
    | Node(color, y, left, right) ->
        if x < y then Node(color, y, delete x left, right) 
        elif x > y then Node(color, y, left, delete x right) 
        else 
            match left, right with
            | Empty, _ -> right 
            | _, Empty -> left
            | _, _ -> 
                let rec minValueNode = function
                    | Node(_, v, Empty, _) -> v
                    | Node(_, _, l, _) -> minValueNode l
                    | Empty -> failwith "empty tree error"
                let minValue = minValueNode right
		balance (color, minValue, left, delete minValue right)

```

4. Фильтрация дерева по предикату: 
```F#
let rec filter predicate = 
    function
    | Empty -> Empty 
    | Node(color, value, left, right) ->
        let leftFiltered = filter predicate left
        let rightFiltered = filter predicate right
        if predicate value then 
            Node(color, value, leftFiltered, rightFiltered)
        else
            match leftFiltered, rightFiltered with
            | Empty, _ -> rightFiltered
            | _, Empty -> leftFiltered
            | _ -> Node(color, value, leftFiltered, rightFiltered)
```

5. Выполнение отображения для ЧКД
```F#
let rec map f = 
    function
    | Empty -> Empty 
    | Node(color, value, left, right) ->
        Node(color, f value, map f left, map f right)
```

6. Свертки (правая и левая)
```F#
let rec foldLeft f acc = 
    function
    | Empty -> acc
    | Node(_, value, left, right) ->
        let leftAcc = foldLeft f acc left
        let rightAcc = foldLeft f (f leftAcc value) right
        rightAcc

let rec foldRight f tree acc =
    match tree with
    | Empty -> acc
    | Node(_, value, left, right) ->
        let rightAcc = foldRight f right acc
        foldRight f left (f value rightAcc)
```

7.  Выполнение свойств моноида: определяется нейтральный элемент и ассоциативность
```F#
let empty = Empty 
let combine t1 t2 = 
	foldLeft (fun acc x -> add x acc) t2 t1
```

## Тестирование

Тестирование проводилось при помощи двух инструментов:

- Xunit - для Unit-тестирования
- FsCheck - для property-based тестирования

## Вывод
В ходе выполнения лабораторной работы я узнала о реализации неизменяемых полиморфных структур данных на языке F#. Это позволило мне лучше понять принципы функционального программирования, такие как использование рекурсивных алгоритмов и различных типов (type). Лабораторная показалась мне сложной, так как реализация некоторых алгоритмов могла бы быть более интуитивной и понятной. В дальнейшем я планирую продолжить углубление в изучении используемых решений.  
