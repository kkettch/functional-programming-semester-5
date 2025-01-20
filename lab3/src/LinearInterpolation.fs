module LinearInterpolation

(*
    Модуль для расчета линейной интерполяции для двух точек с шагом step
*)

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
