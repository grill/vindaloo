module Vindaloo.Architecture

type AddrT = int
type Value = Addr of AddrT | Int of int
type Enviorment = Map<Syntax.Var, AddrT>
type Closure = Syntax.LambdaForm * Enviorment * (Value list)
type Heap = Closure array
type Code = Eval of Syntax.Expr * Enviorment
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
    globals : Enviorment
    code : Code
}

type STGState = Running of STGMachine | Error of string * STGMachine | Finished of STGMachine
