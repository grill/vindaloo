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
    updatestack size = %i
    returnstack size = %i
    heap size = %i
    """
      machine.code
      machine.globals
      machine.argstack
      (List.length machine.updstack)
      (List.length machine.retstack)
      machine.heap.Length

type Step = Not | Once | Finish
type Cmds = Heap of int | Upd of int | Run

let moreInfo cmd machine =
    let cmdparse =
        (str "heap" >>. ws >>. pint32 |>> Heap)
        <|> (str "upd" >>. ws >>. pint32 |>> Upd)
        <|> (str "run" >>% Run)
    match run cmdparse cmd with
      | Success(Heap addr, _ ,_ ) ->
            if addr < machine.heap.Length
            then printfn "%A" machine.heap.[addr]; Not
            else printfn "Heap address out of range"; Not
      | Success(Upd addr, _ ,_ ) ->
            if addr < machine.updstack.Length
            then printfn "%A" machine.updstack.[addr]; Not
            else printfn "Update stack address out of range"; Not
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
            printInfo m' |> ignore
        | Finished _ ->
            printfn "finished"
    printfn "%A" code
    printfn "%A" machine
    printSTG machine
    runstg (Running machine) true



match run binds """
main = {} \n {} -> sum {numbers, 10#} ;

numbers = {} \n {} -> 
    letrec
        count1 = {count1} \n {n} ->
                    case +# {1#, n} of
                      n1 -> let rest = {count1, n1} \u {} -> count1 {n1} in
                            Cons {n, rest}
    in count1 {0#} ;

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
    | Failure(s, _, _) -> printfn "fail\n%s" s; Console.ReadLine () |> ignore


//Threading.Thread.Sleep -1

(* STG Code notepad


mkList = {} \n {n} ->
       case n {} of
          MkInt x# -> 
          
          
main = {} \n {} -> Nil {}

 *)