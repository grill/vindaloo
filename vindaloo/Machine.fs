module Vindaloo.Machine

open FParsec
open Vindaloo.Parser
open Vindaloo.Architecture
open Vindaloo.Util

let valueVar p g v = OptionOr (Map.tryFind v p) (lazy (Map.tryFind v g |> Option.map Addr))

let value p g x =
    match x with
    | Syntax.LiteralA k -> Some (Int k)
    | Syntax.VarA v -> valueVar p g v
    | _ -> None

let rec valueList p g xs f : Option<Value list> =
    match xs with
    | h::tail -> OptionAnd (f p g h) (valueList p g tail f) (fun v vs -> v :: vs)
    | [] -> Some []

(*
let ``let`` h p binds =
    let triple = Map.fold (fun pars name (code : Syntax.LambdaForm) ->
            let updateTriple (g, l, i) ws = (Map.add name (Addr i) g, (code, ws), i+1)
            let ws = valueList p Map.empty code.freeVars valueVar
            OptionAnd pars ws updateTriple) (Some (p, [], Array.length h)) binds
    match triple with
    | Some (p', hn, _) -> Some (p', List.toArray hn |> Array.append h)
    | _ -> None
*)

let EnvironmentAdd hlen p binds =
    let p', _ = Map.fold (fun (g, i) name _ -> (Map.add name (Addr i) g, i+1)) (p, hlen) binds
    p'

let HeapAdd h prhs binds =
    let hn = Map.fold (fun l _ (code : Syntax.LambdaForm) ->
        (fun l ws -> (code, ws) :: l) |>
        OptionAnd l (valueList prhs Map.empty code.freeVars valueVar)) (Some []) binds
    match hn with
    | Some (hn) -> Some (List.toArray hn |> Array.append h)
    | _ -> None

//Perform one computational step of the STG-machine.
//references above the rules are to the STG-machine paper
let step machine : STGState =
    match machine with
    
    //5.2 Applications (1) - tail call
    | { code = Eval (Syntax.ApplE {var = f; pars = xs}, p); globals = g; argstack = a } ->
        match (value p g (Syntax.VarA f), valueList p g xs value) with
        | (Some (Addr addr), Some (vList)) ->
            Running {
            machine with
                code = Enter (addr);
                argstack = List.append vList a
            }
        | _ -> Error ("Tail call failed", machine)
        
    //5.2 Applications (2) - enter function
    | { code = Enter addr; heap = h; globals = g; argstack = a } when Array.length h > addr ->
        match h.[addr] with
        | ({ freeVars = vs; updateable = false; parameters = xs; body = e}, wsf)
            when List.length a >= List.length xs ->
            match List.length xs |> ListSplit a with
            | Some (a', wsa) ->
                let p = List.zip vs wsf |> List.append (List.zip xs wsa) |> Map.ofList
                Running {
                  machine with
                    code = Eval (e, p); 
                    argstack = a'
                }
            | _ -> Error("Enter failed!", machine)
        | _ -> Error("Enter failed!", machine)
        
    //5.3 let(rec) Expressions (3) - let | letrec
    | { code = Eval (Syntax.LetE {binds = binds; expr = e}, p) ; heap = h }
    | { code = Eval (Syntax.LetrecE {binds = binds; expr = e}, p) ; heap = h } ->
        let p' = EnvironmentAdd h.Length p binds
        let prhs =
            match machine.code with
            | Eval (Syntax.LetE {expr = _}, _) -> p
            | _ -> p'
        match HeapAdd h prhs binds with
        | Some (h') ->
            Running {
                machine with
                    code = Eval (e, p'); 
                    heap = h'
            }
        | _ -> Error("Let failed!", machine)

    | _ -> Error ("The supplied state is not vaild", machine) //or the machine is finished

let initSTG code =
    let g, _ = Map.fold (fun (g, i) name _ -> (Map.add name i g, i+1))
                        (Map [], 0) code
    let h = Map.fold (fun h name addr -> ( (Map.find name code) :: h))
                        ([]) g
    {
        argstack = []
        retstack = []
        updstack = []
        heap = List.toArray h
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
