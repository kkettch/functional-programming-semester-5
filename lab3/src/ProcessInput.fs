module ProcessInput

open LinearInterpolation
open NewtonInterpolation

(*
    Модуль для вызова методов интерполяции при достаточном для этого количестве точек 
*)

let processInput (points: seq<float * float>) (step: float) =
    seq {
        let accumulatedPoints =
            Seq.scan
                (fun acc point ->
                    acc @ [ point ])
                [] 
                points

        yield! accumulatedPoints
               |> Seq.collect (fun currentPoints ->

                    let linearInterpolated = 
                        if Seq.length currentPoints >= 2 then
                            let lastTwoPoints = currentPoints |> Seq.rev |> Seq.take 2 |> Seq.rev 
                            lastTwoPoints
                            |> Seq.pairwise
                            |> Seq.collect (fun (p1, p2) ->
                                let interpolatedPoints = linearInterpolation p1 p2 step
                                seq { "linear", interpolatedPoints })
                        else Seq.empty

                    let newtonInterpolated =
                        if Seq.length currentPoints >= 3 then
                            let lastThreePoints = currentPoints |> Seq.rev |> Seq.take 3 |> Seq.rev 
                            let newtonInterpolated = newtonInterpolation lastThreePoints step
                            seq { "newton", newtonInterpolated }
                        else Seq.empty

                    Seq.append linearInterpolated newtonInterpolated
               )
    }