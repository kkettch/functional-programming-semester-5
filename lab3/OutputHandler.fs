module OutputHandler

(* 
    Модуль для вывода точек на дисплей в формате: 
    x0 x1 x2 ... xn
    y0 y1 y2 ... yn
*)

let handleOutput (results: seq<string * seq<float * float>>) =
    results
    |> Seq.iter (fun (methodName, points) ->
        points
        |> Seq.iter (fun (x, y) -> printfn "%s: (%f, %f)" methodName x y))
