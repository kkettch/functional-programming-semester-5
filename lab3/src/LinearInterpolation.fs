module LinearInterpolation

(*
    Модуль для расчета линейной интерполяции для двух точек с шагом step
*)

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