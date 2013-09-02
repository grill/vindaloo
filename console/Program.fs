// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
module Vindaloo.Console

open FParsec
open System
open System.Threading
open System.IO

open Vindaloo.Parser
open Vindaloo.Test.Parser
open Vindaloo.Machine
open Vindaloo.Architecture
open Vindaloo.Cmd



[<EntryPoint>]
let main args =
    let helpMessage = """
    vindaloo.exe [-h | [-d] filename]

        Execute a stg-file with filename.
        
    -h...display help
    -d...start debugger"""
    match Array.toList args with
    | "-h"::_ -> Console.Write helpMessage ; 0
    | f::[] -> parse (File.ReadAllText f) |> runSTG false
    | "-d"::f::[] -> parse (File.ReadAllText f) |> runSTG true
    | _ -> 1
