module Vindaloo.Machine

type Addr = int
type Closure = Syntax.LambdaForm * (Addr list)
type Heap = Map<Addr, Closure>
type Bindings = Map<Syntax.Var, Addr>
type Code = Eval of Syntax.Expr * Bindings
          | Enter of Addr
          | ReturnCon of Syntax.Constr * (Addr list)
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

type Value = Addr | Int of int

type UpdateFrame = {
    argstack : Addr list
    retstack : Continuation list
    closure : Addr
}

type STGMachine = {
    argstack : Addr list
    retstack : Continuation list
    updstack : UpdateFrame list
    heap : Heap
    globals : Bindings
    code : Code
}