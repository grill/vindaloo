module Vindaloo.Machine

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

type STGState = Working of STGMachine | Error of string

let initSTG code =
    let code' = Map.map (fun k v -> (v, [])) code
    let g,_ = Map.fold (fun (g, i) name _ -> (Map.add name i g, i+1))
                        (Map [], 0) code'
    let h = Map.fold (fun h name addr -> (Map.add addr (Map.find name code') h))
                        (Map []) g
    {
        argstack = []
        retstack = []
        updstack = []
        heap = h
        globals = g
        code = Eval (Syntax.ApplE {var = "main"; pars = []}, Map [])
    }

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

let step machine : STGState = match machine with
    | { code = Eval (Syntax.ApplE {var = f; pars = xs}, p);
        globals = g; argstack = a } ->
        match (valueVar p g f) with
        | Some (Addr addr) ->
            match (valueList p g xs) with
            | Some (vList) ->
                Working { machine with
                    code = Enter (addr); 
                    argstack = List.append vList a
                }
            | None -> Error ("")
        | None -> Error ("")
    | _ -> Error ("The supplied state is not vaild")


let eval code =
    initSTG code
