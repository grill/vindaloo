module Vindaloo.Machine

open FParsec
open Vindaloo.Parser
open Vindaloo.Syntax

type AddrT = int
type Value = Addr of AddrT | Int of int
type Closure = Syntax.LambdaForm * (Value list)
type Heap = Map<AddrT, Closure> * int
type LocalBindings = Map<Syntax.Var, Value>
type GlobalBindings = Map<Syntax.Var, AddrT>
type Code = Eval of Syntax.Expr * LocalBindings
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
    globals : GlobalBindings
    code : Code
}

type STGState = Running of STGMachine | Error of string * STGMachine | Finished of STGMachine

let valueVar p g el =
   let pAddr = Map.tryFind el p
   if (pAddr.IsSome) then pAddr else Map.tryFind el g |> Option.map Addr

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

let malloc closure (heap, nextaddr) : Heap =
    (Map.add nextaddr closure heap, nextaddr + 1)

let bind var closure environment heap : LocalBindings * Heap =
    let heap' = malloc closure heap
    let environment' = Map.add var (snd heap' |> Addr) environment
    (environment', heap')
    
let bindrec newbinds env (heap, addr) : LocalBindings * Heap =
    let newaddrs, addr' =
        newbinds |>
        Map.fold
          (fun (env', addr') var _ ->
            Map.add var (addr') env', addr' + 1)
          (Map [], addr)
    let env' =
        newaddrs |>
        Map.fold
          (fun env' var addr ->
            Map.add var (Addr addr) env')
          env
    let newbinds' =
        newbinds |>
        Map.map
          (fun var lf ->
            lf,
            lf.freeVars |> List.map (fun x -> Map.find x env'))
    let heap' =
        newbinds' |>
        Map.fold
          (fun heap' var clos ->
            Map.add (Map.find var newaddrs) clos heap')
          heap
    (env', (heap', addr'))

//Perform one computational step of the STG-machine.
//references above the rules are to the STG-machine paper
let step machine : STGState =
    match machine with
    //5.2 Applications (1)
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
    //5.2 Applications (2)
    | { code = Enter addr; heap = heap,_; globals = g; argstack = a } when heap.ContainsKey addr ->
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
    //5.3 let(rec) Expressions (3)
    | { code = Eval (Syntax.LetE {binds = binds; expr = e}, p); heap = h} ->
        let binds' = 
            binds |>
            Map.map
              (fun _ lf ->
                lf,
                lf.freeVars |> List.map (fun x -> Map.find x p))
        let p', h' =
            Map.fold (fun (p', h') var code ->
                bind var code p' h') (p,h) binds'
        Running {
          machine with
            code = Eval (e, p')
            heap = h'
        }
     | { code = Eval (Syntax.LetrecE {binds = binds; expr = e}, p); heap = h} ->
        let p', h' = bindrec binds p h
        Running {
          machine with
            code = Eval (e, p')
            heap = h'
        }
    | _ -> Error ("The supplied state is not vaild", machine) //or the machine is finished

let initSTG code =
    let g, nextaddr = Map.fold (fun (g, i) name _ -> (Map.add name i g, i+1))
                        (Map [], 0) code
    let h = Map.fold (fun h name addr -> (Map.add addr (Map.find name code) h))
                        (Map []) g
    {
        argstack = []
        retstack = []
        updstack = []
        heap = h, nextaddr
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
