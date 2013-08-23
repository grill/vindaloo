module Vindaloo.Main

open FParsec
open System
open System.Threading

open Vindaloo.Test.Parser
open Vindaloo.Parser
open Vindaloo.Machine
open Vindaloo.Architecture

(*
test binds """
map1 = {} \n {f} ->
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
printfn "asfd"
*)

let printSTG machine =
    printfn """
    code = %A
    globals = %A
    argstack = %A
    updatestack = %A
    returnstack = %A
    heap size = %i
    """
      machine.code
      machine.globals
      machine.argstack
      machine.updstack
      machine.retstack
      machine.heap.Length

let moreInfo cmd machine =
    let heap = str "heap" >>. ws >>. pint32 |>> machine.heap.GetValue
    match run heap cmd with
      | Success(result, _ ,_ ) ->
            printfn "%A" result; true
      | Failure(_, _, _) ->
            false
 
let rec printInfo machine =
    let cmd = Console.ReadLine ()
    if moreInfo cmd machine then
        printInfo machine

let debugSTG code =
    let machine = initSTG code
    let rec runstg m = 
        let mstate = step m
        match mstate with
        | Running m' ->
            printSTG m'
            printInfo m'
            runstg m'
        | Error (msg, m') ->
            printfn "Machine is dead, last state:"
            printfn "%s" msg
            printSTG m'
        | Finished _ ->
            printfn "finished"
    printfn "%A" code
    printfn "%A" machine
    printSTG machine
    runstg machine



match run binds """
main = {} \n {} -> Nil {};

map1 = {} \n {f} ->
       letrec
         mf = {f,mf} \n {xs} ->
              case xs {} of
                Nil {} -> Nil {};
                Cons {y,ys} -> let fy = {f,y} \u {} -> f {y};
                                   mfy = {mf,ys} \u {} -> mf {ys}
                               in Cons {fy,mfy};
                default -> Nil {}
        in mf {}
    """ with
    | Success(result, _, _) -> result |> Map.map (fun _ v -> (v, [])) |> debugSTG
    | Failure(_, _, _) -> printfn "fail"


Threading.Thread.Sleep -1