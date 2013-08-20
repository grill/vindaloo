module Vindaloo.Machine

open FParsec
open Vindaloo.Parser
open Vindaloo.Syntax

type AddrT = int
type Value = Addr of AddrT | Int of int
type Closure = Syntax.LambdaForm * (Value list)
type Heap = Map<AddrT, Closure>
type Bindings = Map<Syntax.Var, AddrT>
type Code = Eval of Syntax.Expr * Bindings
          | Enter of AddrT
          | ReturnCon of Syntax.Constr * (Value list)
          | ReturnInt of int

type Continuation = Syntax.Alts

type UpdateFrame = {
    argstack : Value list
    retstack : Continuation list
    closure : AddrT
}

type STGMachine = {
    argstack : Value list
    retstack : Continuation list
    updstack : UpdateFrame list
    heap : Heap
    globals : Bindings
    code : Code
}

type STGState = Running of STGMachine | Error of string * STGMachine | Finished of STGMachine

let valueVar p g (el : Syntax.Var) =
   let get (map : Map<Syntax.Var, AddrT>) =
      let addr = map.TryFind el
      if (addr.IsSome) then Some (Addr addr.Value) else None
   let pAddr = get p
   if (pAddr.IsSome) then pAddr else get g

let value p g (x : Syntax.Atom) : Option<Value> =
    match x with
    | Syntax.LiteralA k -> Some (Int k)
    | Syntax.VarA v -> valueVar p g v

let rec valueList p g (xs : Syntax.Atoms) : Option<Value list> =
    match xs with
    | h :: tail ->
        match (value p g h) with
        | Some v ->
            match (valueList p g tail) with
            | Some list -> Some (v :: list) 
            | None -> None
        | None -> None
    | [] -> Some []

let splitList list endIdx =
    let rec split l i = match l with
        | h :: tail when i < endIdx ->
            let (alist,elist) = split tail (i+1)
            (h :: alist, elist)
        | xs when i = endIdx -> ([], xs)
        | [] -> ([], [])
        | _ -> ([], []) // --- this should never happen, because length(as) >= length(xs)!!
    split list 0

let step machine : STGState = match machine with
    | { code = Eval (Syntax.ApplE {var = f; pars = xs}, p);
        globals = g; argstack = a } ->
        match (valueVar p g f) with
        | Some (Addr addr) ->
            match (valueList p g xs) with
            | Some (vList) ->
                Running {
                  machine with
                    code = Enter (addr); 
                    argstack = List.append vList a
                }
            | None -> Error ("", machine)
        | Some (Int _) -> Error ("Primitives cannot be applied", machine)
        | None -> Error ("", machine)
    | { code = Enter addr; heap = heap; globals = g; argstack = a } when heap.ContainsKey addr ->
      match Map.find addr heap with
      | ({ freeVars = vs; updateable = false; parameters = xs; body = e}, wsf)
        when List.length a >= List.length xs ->
                let (a', wsa) = List.length xs |> splitList a
                let p = List.zip vs wsf |> List.append (List.zip xs wsa) |> Map.ofList
                Running {
                  machine with
                    code = Eval (e, p); 
                    argstack = a'
                }
      | _ -> Error("Not a correct Enter!", machine)
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
