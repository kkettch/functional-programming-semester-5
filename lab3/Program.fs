module Program

open System
open InputHandler
open ProcessInput
open OutputHandler

[<EntryPoint>]
let main (args: string array) =
    let rec getStep () =
        printfn "step value:"
        match Console.ReadLine() |> System.Double.TryParse with
        | (true, step) -> step 
        | (false, _) ->
            printfn "error: invalid input. correct input: number"
            getStep () 

    let step = getStep ()
    let points = handleInput Console.In 
    processInput points step |> handleOutput
    0
