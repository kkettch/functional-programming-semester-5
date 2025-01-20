module NewtonInterpolation

(*
    Модуль для расчета интерполяции методом Ньютона
*)

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