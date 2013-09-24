module Vindaloo.Graphical

open System
open System.IO
open System.Windows

open Vindaloo.Gui
open Vindaloo.Parser
open Vindaloo.Machine

let helpMessage = """
vindaloo.exe [-h | [-d] filename]

    Execute a stg-file with filename.

-h...display help
-d...start debugger"""


[<STAThread>]
do match Array.toList (Environment.GetCommandLineArgs()) |> List.tail with
    | f::[] ->  Application().Run(mainWindow (parse (File.ReadAllText f) |> Option.get |> initMachine))  |> ignore
    | "-d"::f::[] ->  Application().Run(mainWindow (parse (File.ReadAllText f) |> Option.get |> initMachine)) |> ignore
    | _ -> ignore 0 //Application().Run(mainWindow helpMessage) |> ignore
