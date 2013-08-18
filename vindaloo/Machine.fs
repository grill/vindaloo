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

type ContinuationDefault = {
    var : string option
    body : Syntax.Expr
}

type ConstrContinuation = {
    cases : Map<Syntax.ConstrAppl, Syntax.Expr>
    def : ContinuationDefault
}
type PrimContinuation = {
    cases : Map<Syntax.Literal, Syntax.Expr>
    def : ContinuationDefault
}
type Continuation = ConstrContinuation | PrimContinuation

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