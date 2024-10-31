## Лабораторная работа №2

`ФИО: Чмурова Мария Владиславовна` <br />
`Isu_id: 369027` <br />
`Группа: P3332` <br />

## Задание
Реализовать структуру данных rb-bag

`ФИО: Чмурова Мария Владиславовна` <br />
`Isu_id: 369027` <br />
`Группа: P3332` <br />

### Задание
Реализовать структуру данных rb-bag

### Структура данных: 
*Color* для определения цвета узлов дерева
*Tree*: 
- *Empty* - для пустого дерева, 
- *TreeNode* - для дерева со значением, с указанием цвета и левого/правого поддеревьев 
```F#
type Color = 
    | Red
    | Black
  
type 'a Tree = 
    | Empty
    | Node of 'a TreeNode
and 'a TreeNode = {
    value : 'a; 
    color : Color; 
    left : 'a Tree; 
    right : 'a Tree;
}
```
 
### Реализованные функции

1. Балансировка дерева для поддержания сбалансированности rb-дерева: 
- Возвращение сбалансированности для rb-дерева для всех видов дисбаланса: LL, LR, RL, RR
```F#
let balance color x a b =
    match (color, x, a, b) with
    | (Black, z, // LL imbalance
        Node {
            value = y; 
            color = Red; 
            left = Node {value = x; color = Red; left = a; right = b}; 
            right = c
        }, d)     
    | (Black, z, // LR imbalance
        Node {
            value = x;
            color = Red; 
            left = a; 
            right = Node {value = y; color = Red; left = b; right = c}; 
        }, d)    
    | (Black, x, a, // RL imbalance
        Node { 
            value = z; 
            color = Red; 
            left = Node {value = y; color = Red; left = b; right = c}; 
            right = d; 
        })    
    | (Black, x, a, // RR imbalance
        Node { 
            value = y; 
            color = Red; 
            left = b; 
            right = Node {value = z; color = Red; left = c; right = d}; 
        }) ->
        Node {  
            value = y; 
            color = Red; 
            left = Node {value = x; color = Black; left = a; right = b}; 
            right = Node {value = z; color = Black; left = c; right = d}
        }
    | _ -> Node {value = x; color = color; left = a; right = b}
```

2. Вставка элемента в дерева:
- Элемент добавляется в правое/левое поддерево в зависимости от значения (insertOnCondition)
-  Вершина дерева красится в черный (makeBlack)
```F#
let insert x tree =
    let makeBlack = 
        function
        | Node {value = y; color = _; left = left; right = right} -> Node {value = y; color = Black; left = left; right = right}
        | Empty -> failwith "Unexpected case: tree is empty"

    let rec insertOnCondition tree =
        match tree with
        | Empty -> Node {value = x; color = Red; left = Empty; right = Empty}
        | Node {value = y; color = color; left = a; right = b} ->
            if x < y then balance color y (insertOnCondition a) b
            else balance color y a (insertOnCondition b)

    makeBlack (insertOnCondition tree)
```

3. Удаление элемента из дерева с сохранением сбалансированности:
- Находится узел с искомым элементом для удаления (deleteNode)
- Если у этого узла нет правого поддерева, то левое становится на место удаленного узла
- Иначе становится минимальное значение правого поддерева 
```F#
let rec delete x tree=
    let rec findMin tree =
        match tree with
        | Node {color = _; value = value; left = Empty; right = _} -> value
        | Node {color = _; value = _; left = left; right = _} -> findMin left
        | Empty -> failwith "Unexpected case: tree is empty"

    let rec deleteMin tree =
        match tree with
        | Node {value = _; color = _; left = Empty; right = right} -> right
        | Node {value = value; color = color; left = left; right = right} ->
            balance color value (deleteMin left) right
        | Empty -> failwith "Unexpected case: tree is empty"

    let rec deleteNode tree =
        match tree with
        | Node {value = value; color = color; left = left; right = right} ->
            if x < value then balance color value (deleteNode left) right
            else if x > value then balance color value left (deleteNode right)
            else match right with
                 | Empty -> left
                 | _ -> let minValue = findMin right
                        balance color minValue left (deleteMin right)
        | Empty -> Empty
        
    match tree with
    | Empty -> Empty
    | Node _ -> deleteNode tree
```

4. Фильтрация дерева (filter) по предикату: 
```F#
let rec filter pred tree =
    match tree with
    | Empty -> Empty
    | Node {value = value; color = color; left = left; right = right} ->
        let leftFiltered = filter pred left
        let rightFiltered = filter pred right
        if pred value then
            Node {
	            value = value; 
	            color = color; 
	            left = leftFiltered; 
	            right = rightFiltered
	        }
        else
            combineTrees leftFiltered rightFiltered
```

5. Выполнение отображения (map) для rb-дерева
```F#
let rec map mapFunction tree =
    match tree with
    | Empty -> Empty
    | Node {value = value; color = color; left = left; right = right} ->
        Node { 
            value = mapFunction value; 
            color = color; 
            left = map mapFunction left; 
            right = map mapFunction right 
        }
```

6. Свертки (правая и левая)
```F#
let rec foldLeft f acc tree =
    match tree with
    | Empty -> acc
    | Node {value = value; color = _; left = left; right = right} ->
        let accLeftSubtree = foldLeft f acc left
        let accCounted = f accLeftSubtree value
        foldLeft f accCounted right

let rec foldRight f tree acc =
    match tree with
    | Empty -> acc
    | Node {value = value; color = _; left = left; right = right} ->
        let accRightSubtree = foldRight f right acc
        let accCounted = f accRightSubtree value 
        foldRight f left accCounted
```

7.  Объединение двух деревьев в одно
```F#
let combineTrees left right =
    match left, right with
    | Empty, _ -> right
    | _, Empty -> left
    | Node {value = v; color = c; left = _; right = _} as node, _ ->
        Node {value = v; color = c; left = left; right = right}
```

### Тестирование

Тестирование проводилось при помощи двух инструментов:

- Xunit - для Unit-тестирования
- FsCheck - для property-based тестирования
- 
### Вывод
В ходе выполнения лабораторной работы я узнала о реализации неизменяемых полиморфных структур данных на языке F#. Я снова использовала такие принципы функционального программирования, как использование рекурсивных алгоритмов и замечаний (type). Лабораторная показалась мне сложной, так как реализация некоторых алгоритмов могла бы быть более интуитивной и понятной. В дальнейшем я планирую продолжить углубление в изучении используемых решений.  
