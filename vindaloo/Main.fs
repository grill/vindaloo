module Vindaloo.Main

open FParsec
open System
open System.Threading

open Vindaloo.Test.Parser
open Vindaloo.Parser
open Vindaloo.Machine

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

let debugSTG code =
    let machine = initSTG code
    let rec runstg m = 
        let mstate = step m
        match mstate with
        | Running m' ->
            printfn "%A" m'
            runstg m'
        | Error (msg, m') ->
            printfn "Machine is dead, last state:"
            printfn "%s" msg
            printfn "%A" m'
        | Finished _ ->
            printfn "finished"
    printfn "%A" machine
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