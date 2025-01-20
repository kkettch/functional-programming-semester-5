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
                (fun (firstPoint, buffer) point ->
                    match firstPoint with
                    | [] -> 
                        ( [point], [point] )  
                    | _ -> 
                        let newBuffer = 
                            if List.length buffer < 3 then 
                                buffer @ [point]  
                            else 
                                buffer.Tail @ [point]  

                        (firstPoint, newBuffer) 
                ) 
                ([], [])  
                points
            
        yield! accumulatedPoints
               |> Seq.collect (fun currentPoints -> 
                    let linearInterpolated = 
                        if List.length (snd currentPoints) >= 2 then
                            let lastTwoPoints = (snd currentPoints) |> List.rev |> List.take 2 |> List.rev 
                            let firstNumber = (fst (List.head (fst currentPoints)))
                            lastTwoPoints
                            |> Seq.pairwise
                            |> Seq.collect (fun (p1, p2) ->   
                                let interpolatedPoints = linearInterpolation p1 p2 step firstNumber
                                seq { "linear", interpolatedPoints })
                        else Seq.empty

                    let newtonInterpolated =
                        if List.length (snd currentPoints) >= 3 then
                            let firstNumber = (fst (List.head (fst currentPoints)))
                            let newtonInterpolated = newtonInterpolation (snd currentPoints) step firstNumber
                            seq { "newton", newtonInterpolated }
                        else Seq.empty

                    Seq.append linearInterpolated newtonInterpolated
               )
    }