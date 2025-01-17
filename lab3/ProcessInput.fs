module ProcessInput

open LinearInterpolation

(*
    Модуль для вызова методов интерполяции при достаточном для этого количестве точек 
*)

let processInput (points: seq<float * float>) (step: float) =
    seq {
        let accumulatedPoints =
            Seq.scan
                (fun acc point ->
                    match acc with
                    | _ when List.length acc >= 2 -> acc.Tail @ [ point ]
                    | _ -> acc @ [ point ])
                []
                points

        yield! accumulatedPoints
               |> Seq.filter (fun currentPoints -> List.length currentPoints >= 2)
               |> Seq.collect (fun currentPoints ->
                    let lastTwoPoints = currentPoints |> Seq.rev |> Seq.take 2 |> Seq.rev |> Seq.toList
                    lastTwoPoints
                    |> Seq.pairwise
                    |> Seq.collect (fun (p1, p2) ->
                        let interpolatedPoints = linearInterpolation p1 p2 step
                        seq { "linear", interpolatedPoints }))
    }
