module Vindaloo.Cmd

open FParsec
open System
open System.Threading
open System.IO

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
type Cmds = Heap of int | Upd of int | Run | RunV | Help

let moreInfo cmd machine =
    let cmdparse =
        (str "heap" >>. ws >>. pint32 |>> Heap)
        <|> (str "upd" >>. ws >>. pint32 |>> Upd)
        <|> (str "run" >>% Run)
        <|> (str "runv" >>% RunV)
        <|> (str "help" >>% Help)
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
      | Success(Help, _, _) ->
            Console.WriteLine """
        press Enter ... apply rule (last step closes the debugger)

    Commands:
        help ... Help Message
        run ... run program without text output for each step
        runv ... run program verbose with text output for each step
        heap addr ... get heap frame at address addr
        upd pos ... get update stack frame at position nr""" ; Not
      | Failure(_, _, _) ->
            Once
 
let rec printInfo machine =
    Console.Write "\ndebug> "
    let cmd = Console.ReadLine ()
    match moreInfo cmd machine with
      | Not -> printInfo machine
      | Once -> true, false
      | Finish silent -> false, silent

let rec runMachine mstate do_info silent fin_silent = 
    match mstate with
    | Running m' ->
        if not silent
        then printSTG m'
        let next_info, next_silent =
            if do_info
            then printInfo m'
            else false, silent
        runMachine (step m') next_info next_silent fin_silent
    | Error (msg, m') ->
        printfn "Machine is dead, last state:"
        printfn "%s" msg
        printSTG m'
        printInfo m' |> ignore ; m'
    | Finished m' ->
        if not fin_silent then
            printSTG m'
            printInfo m' |> ignore
        m'

let debugger code =
    let machine = initMachine code
    Console.WriteLine """Debugger has been started."""
    printSTG machine
    runMachine (Running machine) true false false

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let interpreter code =
    let helpMessage = """
    Commands:
        help; ... Help Message
        run filepath; ... run a stg file
        load filepath; ... add bindings of stg file to the enviorment
        debug filepath; ... debug a stg file
        heap addr; ... get heap frame at address addr
        upd pos; ... get update stack frame at position nr
        print; ... print Machine
        exit; ... Exit interpreter"""
    let machine = { initMachine code with code = ReturnInt 0 }
    let runMachine m =  runMachine m false true false
    let rec readCode () =
        let line = Console.ReadLine()
        if line.EndsWith(";")
        then line.Remove(line.Length-1)
        else line + readCode ()
        (*let key : ConsoleKeyInfo = Console.ReadKey()
        if key.KeyChar.ToString() = Environment.NewLine &&
            not (key.Modifiers = ConsoleModifiers.Shift)
        then ""
        else key.KeyChar.ToString () + readCode ()*)
    let rec run (machine : STGMachine) =
        Console.Write "\n> "
        match readCode () with
        | "exit" -> 0
        | "help" -> Console.WriteLine helpMessage ; run machine
        | "print" -> printSTG machine; run machine
        | Prefix "heap " addr ->
            try
                if int addr < machine.heap.Length
                then printfn "%A" machine.heap.[int addr]
                else printfn "Heap address out of range" |> ignore
            with | :? System.FormatException -> Console.Write "You have to specify a valid number!"
            run machine
        | Prefix "upd " pos ->
            try
                if int pos < machine.updstack.Length
                then printfn "%A" machine.updstack.[int pos]
                else printfn "Update stack address out of range" |> ignore
            with | :? System.FormatException -> Console.Write "You have to specify a valid number!"
            run machine
        | Prefix "run " filepath ->
            parse (File.ReadAllText filepath) |>
            Option.map (fun x -> initMachine x |> Running |> runMachine) |> ignore
            run machine
        | Prefix "debug " filepath ->
            parse (File.ReadAllText filepath) |>
            Option.map debugger |> ignore
            run machine
        | Prefix "load " filepath ->
            match parse (File.ReadAllText filepath) with
            | Some r -> extendMachine machine r |> run
            | None -> run machine
        | code ->
            match parseAppl code with
            | Some appl ->
                {
                    (Running { machine with code = Eval (appl, Map []) } |> runMachine) with
                        argstack = []
                        retstack = []
                        updstack = []
                } |> run
            | None ->
                match parse code with
                | Some binds -> binds |> extendMachine machine |> run
                | _ -> run machine
    Console.WriteLine """
    Welcome to the STG Machine interpreter
    Type "help;" to get a list of all available commands
    Each command, binding or application has to end with ;
    """ 
    run machine