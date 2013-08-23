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

let EnvironmentAdd hlen p binds =
    let p', _ = Map.fold (fun (g, i) name _ -> (Map.add name (Addr i) g, i+1)) (p, hlen) binds
    p'

let HeapAdd h prhs binds =
    let hn = Map.fold (fun l _ (code : Syntax.LambdaForm) ->
        (fun l ws -> (code, ws) :: l) |>
        OptionAnd l (OptionListMap prhs Map.empty code.freeVars valueVar)) (Some []) binds
    match hn with
    | Some (hn) -> Some (List.toArray hn |> Array.append h)
    | _ -> None

//Perform one computational step of the STG-machine.
//references above the rules are to the STG-machine paper
let step machine : STGState =
    match machine with
    
    //5.2 Applications (1) - tail call
    | { code = Eval (Syntax.ApplE {var = f; pars = xs}, p); globals = g; argstack = a } ->
        match (value p g (Syntax.VarA f), OptionListMap p g xs value) with
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
        | _ -> Error("Let(rec) failed!", machine)
        
    //5.4 Case expressions and data constructors (4) - case
    | { code = Eval (Syntax.CaseE {alts = alts; expr = e}, p) ; retstack = rs } ->
        Running {
            machine with
                code = Eval (e, p);
                retstack = (alts, p) :: rs
        }

    //5.4 Case expressions and data constructors (5) - evaluate constructor
    | { code = Eval (Syntax.ConstrApplE {constr = c; pars = xs}, p) ; globals = g } ->
        match OptionListMap p g xs value with
        | Some vs ->
            Running {
                machine with code = ReturnCon (c, vs)
            }
        | _ -> Error ("Evaluate Constructor failed!", machine)

    | _ -> Error ("The supplied state is not vaild", machine) //or the machine is finished

let initSTG code =
    let g, _ = Map.fold (fun (g, i) name _ -> (Map.add name i g, i+1))
                        (Map [], 0) code
    let h = Map.foldBack (fun name addr h -> ( (Map.find name code) :: h))
                        g ([])
    {
        argstack = []
        retstack = []
        updstack = []
        heap = h |> List.toArray
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
