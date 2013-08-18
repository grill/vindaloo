module Vindaloo.Syntax

(** Syntax of the STG language **

Program: prog --> binds

Bindings: binds --> var(1) = lf(1); ... ; var(n) = lf(n)    n >= 1

Lambda-forms: lf --> vars(f) \pi vars(a) -> expr

Update flag: pi --> u | n

Expression: expr --> let binds in expr
                  | letrec binds in expr
                  | case expr of alts
                  | var atoms
                  | constr atoms
                  | prim atoms
                  | literal

Alternatives: alts --> aalt(1); ... ; aalt(n); default    n >= 0 (Algebraic)
                    | palt(1); ... ; palt(n); default    n >= 0 (Primitive)

Algebraic alt: aalt --> constr vars -> expr
Primitive alt: palt --> literal -> expr
Default alt: default --> var -> expr
                      | default -> expr

Literals: literal --> 0# | 1# | ...
                    | ...

Primitive ops: prim --> +# | -# | *# | /#
                      | ...

Variable lists: vars --> {var(1), ... , var(n)}    n >= 0

Atom lists: atoms --> {atom(1), ..., atom(n)}    n >= 0
Atom: atom --> var | literal

*)

type Bindings = Map<Var, LambdaForm>
and LambdaForm = {
    freeVars : Vars;
    updateable : bool;
    parameters : Vars;
    body : Expr;
}
and Operator = int -> int -> int
and Vars = List<Var>
and Var = string
and Literal = int
and Atom = Var | Literal
and Constr = string
and Let = {
    binds : Bindings
    expr : Expr
}
and Letrec = {
    binds : Bindings
    expr : Expr
}
and Case = {
    expr : Expr
    alts : Alts
}
and Appl = {
    var : Var
    pars : List<Atom>
}
and ConstrAppl = {
    constr : Constr
    pars : List<Atom>
}
and PrimAppl = {
    fu : int -> int -> int
    pars : List<Atom>
}
and Expr = Let | Letrec | Case | Appl | ConstrAppl | PrimAppl | Literal
and AAlt = {
    case : ConstrAppl
    body : Expr
}
and PAlt = {
    case : Literal
    body : Expr
}
and NDAlt = {
    body : Expr
}
and VDAlt = {
    var : Var
    body : Expr
}
and DAlt = NDalt | VDalt
and AAlts = {
    alts : List<AAlt>
    def : DAlt
}
and PAlts = {
    alts : List<PAlt>
    def : DAlt
}
and Alts = AAlts | PAlts