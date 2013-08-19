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