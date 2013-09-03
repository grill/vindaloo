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

let library = """
sum = {} \n {list, n} ->
    case n {} of
        0# -> MkInt {0#};
        i  -> case list {} of
                Cons {x, xs} ->
                    case -# {i, 1#} of
                      i1 -> let next = {xs, i1} \u {} -> sum {xs, i1} in
                            case next {} of
                                MkInt {x2} -> case +# {x, x2} of s -> MkInt {s}
                                default -> MkInt {0#}
                default -> MkInt {0#} ;

map = {} \n {f} ->
       letrec
         mf = {f,mf} \n {xs} ->
              case xs {} of
                Nil {} -> Nil {};
                Cons {y,ys} -> let fy = {f,y} \u {} -> f {y};
                                   mfy = {mf,ys} \u {} -> mf {ys}
                               in Cons {fy,mfy};
                default -> Nil {}
        in mf {}
"""

let unpack f o =
    match o with
    | Some x -> f x
    | None -> 0

[<EntryPoint>]
let main args =
    let helpMessage = """
    vindaloo.exe [-h | -i | [-d] filename]

        Execute a stg-file with filename.
        
    -h...display help
    -d...start debugger
    -i...interpreter"""

    match Array.toList args with
    | "-h"::_ -> Console.Write helpMessage ; 0
    | "-i"::[] -> parse library |> unpack interpreter
    | f::[] ->
        parse (File.ReadAllText f) |> 
        unpack (fun x -> runMachine (initMachine x |> Running) false true true |> ignore ; 0)
    | "-d"::f::[] -> parse (File.ReadAllText f) |> unpack (fun x -> debugger x |> ignore ; 0)
    | _ -> 1
