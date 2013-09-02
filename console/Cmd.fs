module Vindaloo.Cmd

open FParsec
open System
open System.Threading

open Vindaloo.Test.Parser
open Vindaloo.Parser
open Vindaloo.Machine
open Vindaloo.Architecture

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

type Step = Not | Once | Finish of bool
type Cmds = Heap of int | Upd of int | Run | RunV

let moreInfo cmd machine =
    let cmdparse =
        (str "heap" >>. ws >>. pint32 |>> Heap)
        <|> (str "upd" >>. ws >>. pint32 |>> Upd)
        <|> (str "run" >>% Run)
        <|> (str "runv" >>% RunV)
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
            Finish true
      | Success(RunV, _, _) ->
            Finish false
      | Failure(_, _, _) ->
            Once
 
let rec printInfo machine =
    let cmd = Console.ReadLine ()
    match moreInfo cmd machine with
      | Not -> printInfo machine
      | Once -> true, false
      | Finish silent -> false, silent

let runSTG debug code =

    match code with
    | None -> 1
    | Some code ->

    let machine = initSTG code
    let rec runSTG mstate do_info silent = 
        match mstate with
        | Running m' ->
            if not silent
            then printSTG m'
            let next_info, next_silent =
                if do_info
                then printInfo m'
                else false, silent
            runSTG (step m') next_info next_silent
        | Error (msg, m') ->
            printfn "Machine is dead, last state:"
            printfn "%s" msg
            printSTG m'
            printInfo m' |> ignore ; 1
        | Finished m' ->
            if not silent then
                printfn "finished"
                printSTG m'
                printInfo m' |> ignore
            0
    if debug then
        printfn "%A" code
        printfn "%A" machine
        printSTG machine
    runSTG (Running machine) debug (not debug)
