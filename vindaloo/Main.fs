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

type Step = Not | Once | Finish
type Cmds = Heap of int | Run

let moreInfo cmd machine =
    let cmdparse =
        (str "heap" >>. ws >>. pint32 |>> Heap)
        <|> (str "run" >>% Run)
    match run cmdparse cmd with
      | Success(Heap addr, _ ,_ ) ->
            printfn "%A" machine.heap.[addr]; Not
      | Success(Run, _, _) ->
            Finish
      | Failure(_, _, _) ->
            Once
 
let rec printInfo machine =
    let cmd = Console.ReadLine ()
    match moreInfo cmd machine with
      | Not -> printInfo machine
      | Once -> true
      | Finish -> false

let debugSTG code =
    let machine = initSTG code
    let rec runstg mstate do_info = 
        match mstate with
        | Running m' ->
            printSTG m'
            let next_info =
                if do_info
                then printInfo m'
                else false
            runstg (step m') next_info
        | Error (msg, m') ->
            printfn "Machine is dead, last state:"
            printfn "%s" msg
            printSTG m'
        | Finished _ ->
            printfn "finished"
    printfn "%A" code
    printfn "%A" machine
    printSTG machine
    runstg (Running machine) true



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