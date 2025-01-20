## Лабораторная работа №3

`ФИО: Чмурова Мария Владиславовна` <br />
`Isu_id: 369027` <br />
`Группа: P3332` <br />

## Задание
Повторно реализовать лабораторную работу по предмету "Вычислительная математика" посвящённую интерполяции

## Выполнение 

Реализованные алгоритмы: линейная интерполяция и интерполяция методом Ньютона

Программа использует различные модули: 
- Для обработки входного потока (InputHandler)
- Для обработки алгоритмов интерполяции (ProcessInput)
- Соответственно алгоритмы (LinearInterpolation и NewtonInterpolation)
- Обработка выходного потока (OutputHandler)

### Линейная интерполяция: 
```F#
let rec linearInterpolation (x0, y0) (x1, y1) (step: float) (minX: float) =
    if minX < x0 then
        linearInterpolation (x0, y0) (x1, y1) step (minX + step)
    else
        if minX > x1 then
            Seq.empty
        else
            let x = minX
            let y = (y0 * (x1 - x) + y1 * (x - x0)) / (x1 - x0)
            seq {
                yield (x, y)
                yield! linearInterpolation (x, y) (x1, y1) step (minX + step)
            }
```

### Метод Ньютона: 
```F#
let newtonInterpolation points step minX =

    let rec muliplyingX points currentX =
        match Seq.length points with
            | 1 -> (currentX - fst (Seq.last points))
            | _ -> (currentX - fst (Seq.last points)) * (muliplyingX (Seq.take ((Seq.length points) - 1) points) currentX)

    let rec dividedDifference points currentX =
        match Seq.length points with
        | 1 -> (currentX - fst (Seq.head points))
        | _ -> (currentX - fst (Seq.head points)) * (dividedDifference (Seq.tail points) currentX)

    let rec coefficients points amountOfPoints:float =
        let swap sequence =
            Seq.append (Seq.tail sequence) (Seq.singleton (Seq.head sequence))
        match amountOfPoints with
        | 1 -> (snd (Seq.head points)) / (dividedDifference (Seq.tail points) (fst (Seq.head points)))
        | _ -> (snd (Seq.head points)) / (dividedDifference (Seq.tail points) (fst (Seq.head points))) + (coefficients (swap points) (amountOfPoints-1))
    
    let rec newtonInterpolationFunc points countedX =
        let deleteLast someSeq = 
            Seq.take ((Seq.length someSeq) - 1) someSeq
        match Seq.length points with
        | 1 -> snd (Seq.head points)
        | _ -> 
            let countedСoefficient = coefficients points (Seq.length points)
            let currentxFunc = muliplyingX (deleteLast points) countedX
            (countedСoefficient)*(currentxFunc) + (newtonInterpolationFunc (deleteLast points) countedX)

    let rec newtonInterpolationRecursive points xMin xMax step minX =
        if minX < xMin then
            newtonInterpolationRecursive points xMin xMax step (minX + step)
        else if minX > xMax then
            Seq.empty
        else
            let countedPoint = (minX, newtonInterpolationFunc points minX)
            let nextPoints = newtonInterpolationRecursive points xMin xMax step (minX + step)
            Seq.append (Seq.singleton countedPoint) nextPoints

    let xMin = points |> Seq.minBy fst |> fst
    let nextXMax = (points |> Seq.maxBy fst |> fst) + step  
    newtonInterpolationRecursive points xMin nextXMax step minX
```

### Пример работы программы: 

<img width="454" alt="Снимок экрана 2025-01-20 в 19 31 33" src="https://github.com/user-attachments/assets/48a96673-63da-4595-95fa-42de105bc6c6" />

