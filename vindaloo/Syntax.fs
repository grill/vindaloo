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
and Binding = Var * LambdaForm
and Updateable = bool
and LambdaForm = {
    freeVars : Vars;
    updateable : Updateable;
    parameters : Vars;
    body : Expr;
}
and Operator = int -> int -> int
and Vars = List<Var>
and Var = string
and Literal = int
and Atom = VarA of Var | LiteralA of Literal
and Atoms = Atom list
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
    pars : Atoms
}
and ConstrAppl = {
    constr : Constr
    pars : Atoms
}
and PrimAppl = {
    op : Operator
    pars : Atoms
}
and Expr = LetE of Let
         | LetrecE of Letrec
         | CaseE of Case
         | ApplE of Appl
         | ConstrApplE of ConstrAppl
         | PrimApplE of PrimAppl
         | LiteralE of Literal
and AlgebraicAlt = {
    case : ConstrAppl
    body : Expr
}
and PrimitiveAlt = {
    case : Literal
    body : Expr
}
and SimpleDefaultAlt = {
    body : Expr
}
and VarDefaultAlt = {
    var : Var
    body : Expr
}
and DefaultAlt = SimpleDefault of SimpleDefaultAlt | VarDefault of VarDefaultAlt
and AlgebraicAltList = {
    alts : List<AlgebraicAlt>
    def : DefaultAlt
}
and PrimitiveAltList = {
    alts : List<PrimitiveAlt>
    def : DefaultAlt
}
and Alts = AlgebraicAlts of AlgebraicAltList | PrimitiveAlts of PrimitiveAltList
