module Vindaloo.Machine

open FParsec
open Vindaloo.Parser
open Vindaloo.Architecture
open Vindaloo.Util

let valueVar p g x = 
    let getAddr el map = Map.tryFind el map |> Option.map Addr
    match getAddr x p with
    | None -> getAddr x g
    | x -> x

let value p g x =
    match x with
    | Syntax.LiteralA k -> Some (Int k)
    | Syntax.VarA v -> valueVar p g v
    | _ -> None

let rec valueList p g (xs : Syntax.Atoms) : Option<Value list> =
    match xs with
    | h::tail -> OptionAnd (value p g h) (valueList p g tail) (fun v vs -> Some (v :: vs))
    | [] -> Some []

let getMapVarToAddr code =
    let g,_ = Map.fold (fun (g, i) name _ -> (Map.add name i g, i+1))
                   (Map [], 0) code
    g

let getMapAddrToClosure code g prhs =
    Map.fold (fun h name addr -> (Map.add addr (Map.find name code) h)) (Map []) g

let step machine : STGState =
    match machine with

    //(1) Tail Call
    | { code = Eval (Syntax.ApplE {var = f; pars = xs}, p); globals = g; argstack = a } ->
        match (value p g (Syntax.VarA f), valueList p g xs) with
        | (Some (Addr addr), Some (vList)) ->
            Running {
            machine with
                code = Enter (addr);
                argstack = List.append vList a
            }
        | _ -> Error ("Tail call failed", machine)

    //(2) Enter Function
    | { code = Enter addr; heap = h; globals = g; argstack = a } when Array.length h > addr ->
        match h.[addr] with
        | ({ freeVars = vs; updateable = false; parameters = xs; body = e}, _, wsf)
            when List.length a >= List.length xs ->
            match List.length xs |> splitList a with
            | Some (a', wsa) ->
                let p = List.zip vs wsf |> List.append (List.zip xs wsa) |> Map.ofList
                Running {
                  machine with
                    code = Eval (e, p);
                    argstack = a'
                }
            | _ -> Error("Enter failed!", machine)
        | _ -> Error("Enter failed!", machine)

    //(3) let (rec)
    | { code = Eval (Syntax.LetE {binds = binds; expr = e}, p) } ->
        let p',_ = Map.fold (fun (g, i) name _ -> (Map.add name i g, i+1))
                   (p, 0) binds
        let g = getMapVarToAddr p
        let prhs = p
        let h' = getMapAddrToClosure binds g prhs
        let p' = Map.toArray p |> Array.concat (Map.toArray g) |> Map.ofArray
        Running {
            machine with
                code = Eval (e, p'); 
                heap = h'
        }
    | _ -> Error ("The supplied state is not vaild", machine) //or the machine is finished

let initSTG code =
    let g,_ = Map.fold (fun (g, i) name _ -> (Map.add name i g, i+1))
                        (Map [], 0) code
    let h = Map.fold (fun h name addr -> (Map.add addr (Map.find name code) h))
                        (Map []) g
    {
        argstack = []
        retstack = []
        updstack = []
        heap = h
        globals = g
        code = Eval (Syntax.ApplE {var = "main"; pars = []}, Map [])
    }

(* abstract machine *)
(*let eval code =
    match (run prog code) with
    | Success(result, _, _)   ->
        initSTG result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg
*)

(* repl *)
(*
let stgmachine =
    let runmachine = init
    match System.Console.In.ReadLine() with
    | null -> p
    | line -> eval line state
  *)
