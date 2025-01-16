module Program

open System

open InputHandler
open ProcessInput
open OutputHandler

[<EntryPoint>]
let main (args: string array) =
    let points = handleInput Console.In
    processInput points
    0