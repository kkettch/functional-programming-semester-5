module OutputHandler

open System

(* 
    Модуль для вывода точек на дисплей
*)

let handleOutput (results: seq<string * seq<float * float>>) =
    results
    |> Seq.iter (fun (methodName, points) ->
        printfn "%s:" methodName
        points
        |> Seq.iter (fun (x, y) -> printfn "  (%0.2f, %0.2f)" x y))
