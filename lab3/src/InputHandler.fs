module InputHandler

open System
open LinearInterpolation

(*
    Модуль для обработки ввода с консоли
*)

let parseInput (input : string) =
    input
    |> fun s -> s.Trim()  
    |> (fun s -> s.Replace("\t", " ")) 
    |> (fun s -> System.Text.RegularExpressions.Regex.Replace(s, @"\s+", " ")) 
    |> fun s -> s.Split(' ')

let handleInput (reader: System.IO.TextReader) =
    seq {
        while true do
            printfn "enter point:"
            match reader.ReadLine() with
            | null -> yield! Seq.empty
            | input ->
                let values = parseInput input
                if values.Length = 2 then
                    yield float values.[0], float values.[1]
                else
                    printfn "error: invalid input. correct input: X Y"
    }