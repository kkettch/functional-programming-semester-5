module Program

open System
open System.Globalization
open InputHandler
open ProcessInput
open OutputHandler

[<EntryPoint>]
let main (args: string array) =

    let rec getStep () =
        printfn "step value:"
        let input = Console.ReadLine()
        match Double.TryParse(input, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | (true, step) -> step
        | (false, _) ->
            printfn "error: invalid input. correct input: number"
            getStep ()

    let step = getStep ()
    let points = handleInput Console.In 
    processInput points step |> handleOutput
    0

