//namespace vindaloo

module Vindaloo.Parser

open FParsec

let one = 1

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