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