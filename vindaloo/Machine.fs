module Machine

type Addr = int
type Closure = Syntax.LambdaForm * (Addr list)
type Heap = Map<Addr, Closure>
type Bindings = Map<Syntax.Var, Addr>
type Code = Eval of Syntax.Expr * Bindings
          | Enter of Addr
          | ReturnCon of Syntax.Constr * (Addr list)
          | ReturnInt of int


type ConstrContinuation = {
    cases : Map<Syntax.AAlt, Syntax.Expr>
    def : Syntax.DAlt
}
type PrimContinuation = {
    cases : Map<Syntax.PAlt, Syntax.Expr>
    def : Syntax.DAlt
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