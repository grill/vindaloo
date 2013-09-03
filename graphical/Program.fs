module Vindaloo.Graphical

open System
open System.IO
open System.Windows

open Vindaloo.Gui
open Vindaloo.Parser

let helpMessage = """
vindaloo.exe [-h | [-d] filename]

    Execute a stg-file with filename.

-h...display help
-d...start debugger"""


[<STAThread>]
do match Array.toList (Environment.GetCommandLineArgs()) |> List.tail with
    | f::[] ->  Application().Run(mainWindow (File.ReadAllText f))  |> ignore
    | "-d"::f::[] ->  Application().Run(mainWindow "Debug") |> ignore
    | _ -> Application().Run(mainWindow helpMessage) |> ignore
