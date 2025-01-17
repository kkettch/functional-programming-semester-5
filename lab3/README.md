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
let rec linearInterpolation (x0, y0) (x1, y1) (step : float) =
    if x0 > (x1 + step) then
        Seq.empty 
    else
        let x = x0 + step
        let y = (y0 * (x1 - x) + y1 * (x - x0)) / (x1 - x0)
        seq {
            yield (x0, y0)
            yield! linearInterpolation (x0 + step, y) (x1, y1) step
        }
```

### Метод Ньютона: 
```F#
let newtonInterpolation points step =

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

    let rec newtonInterpolationRecursive points xMin xMax step =
        if xMin > xMax then
            Seq.empty
        else
            let countedPoint = (xMin, newtonInterpolationFunc points xMin)
            let nextPoints = newtonInterpolationRecursive points (xMin + step) xMax step
            Seq.append (Seq.singleton countedPoint) nextPoints

    let xMin = points |> Seq.minBy fst |> fst
    let nextXMax = (points |> Seq.maxBy fst |> fst) + step  
    newtonInterpolationRecursive points xMin nextXMax step
```

### Пример работы программы: 

<img width="599" alt="Снимок экрана 2025-01-17 в 16 52 00" src="https://github.com/user-attachments/assets/0e901b71-0ba1-46c1-b312-29a2d048c76d" />
