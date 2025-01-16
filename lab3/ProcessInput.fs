module ProcessInput

open LinearInterpolation

(*
    Модуль для вызова методов интерполяции при достаточном для этого количестве точек 
*)

let processInput points =
    points
    |> Seq.fold (fun lastTwo (x, y) ->
        let updatedLastTwo = (x, y) :: lastTwo |> List.truncate 2
        match updatedLastTwo with
        | [(x1, y1); (x2, y2)] ->
            printfn "last 2 points: %.2f %.2f и %.2f %.2f" x2 y2 x1 y1
        | _ -> ()
        updatedLastTwo
    ) []
    |> ignore