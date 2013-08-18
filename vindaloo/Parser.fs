module Vindaloo.Parser

open FParsec
open Vindaloo.Syntax

let ws = spaces
let str = pstring
let list p = between (str "{") (str "}") (
       ((sepBy (p .>> ws) (str "," .>> ws))) <|>
       (ws >>. preturn(List.empty))
    )

//Literals: literal --> 0# | 1# | ...
let literal : Parser<Literal, unit> = pint32 .>> str "#"

//Primitive ops: prim --> +# | -# | *# | /#
let prim : Parser<Operator, unit> =
    (str "+#" >>. preturn(+)) <|>
    (str "-#" >>. preturn(-)) <|>
    (str "*#" >>. preturn(*)) <|>
    (str "/#" >>. preturn(/))

let isVarStart c = isAsciiLower c
let var : Parser<Var, unit> =
    identifier (IdentifierOptions(isAsciiIdStart = isVarStart))

//Variable lists: vars --> {var(1), ... , var(n)}    n >= 0
let vars : Parser<Vars, unit> = list var

//Atom: atom --> var | literal
let atom : Parser<Atom, unit> =
    (var |>> VarA) <|>
    (literal |>> LiteralA)

//Atom lists: atoms --> {atom(1), ..., atom(n)}    n >= 0
let atoms : Parser<Atoms, unit> = list atom

//Bindings: binds --> var(1) = lf(1); ... ; var(n) = lf(n)    n >= 1
//let bind = var .>> ws .>> str "=" .>> ws .>>. lf .>> ws
//let binds = bind .>>? str ";" .>>. sepBy bind (str ";")

//Update flag: pi --> u | n
let pi : Parser<Updateable, unit> =
    (str "u" >>. preturn(true)) <|>
    (str "n" >>. preturn(false))

let appl : Parser<Expr, unit> =
    var .>> ws .>>. atoms |>>
    fun (var, pars) -> ApplE { var = var; pars = pars }

let isConstrStart c = isAsciiUpper c
let constr : Parser<Constr, unit> =
    identifier (IdentifierOptions(isAsciiIdStart = isConstrStart))
let constrAppl : Parser<ConstrAppl, unit> =
    constr .>> ws .>>. atoms |>>
    fun (constr, pars) -> { constr = constr; pars = pars }

let primAppl : Parser<Expr, unit> =
    prim .>> ws .>>. atoms |>>
    fun (op, pars) -> PrimApplE {op = op; pars = pars}

//Expression: expr --> let binds in expr
//                  | letrec binds in expr
//                  | case expr of alts
//                  | var atoms
//                  | constr atoms
//                  | prim atoms
//                  | literal

let expr : Parser<Expr, unit> =
//    (str "let" >>. binds .>> ws .>> str "in" .>> ws .>> expr) <|>
//    (str "letrec" >>. binds .>> ws .>> str "in" .>> ws .>> expr) <|>
//    (str "case" >>. expr .>> ws .>> str "of" .>> ws .>> alts) <|>
    (appl) <|>
    (constrAppl |>> ConstrApplE) <|>
    (primAppl) <|>
    (literal |>> LiteralE)
    

let ``->`` = ws .>> str "->" .>> ws

//Lambda-forms: lf --> vars(f) \pi vars(a) -> expr
//let lf : Parser<LambdaForm, unit> =
//   vars .>> str "\\" .>>. pi .>>. vars .>> ``->`` .>>. expr

//Default alt: default --> var -> expr
//                      | default -> expr
let dalt : Parser<DefaultAlt, unit> =
    (str "default" |>> (fun _ -> None) <|> (var |>> Some)) .>> ``->`` .>>. expr
        |>> fun (var, expr) -> {var=var; body=expr}


//generic matching alternative
let galt x = x .>> ``->`` .>>. expr

//Primitive alt: palt --> literal -> expr
let palt = galt literal

//Algebraic alt: aalt --> constr vars -> expr
let aalt = galt constrAppl

//Alternatives: alts --> aalt(1); ... ; aalt(n); default    n >= 0 (Algebraic)
//                   | palt(1); ... ; palt(n); default    n >= 0 (Primitive)
let galts xalt = sepEndBy xalt (str ";") .>> ws .>>. dalt
                 |>> fun (alts, def) -> {cases = Map.ofList alts; def = def}
let alts : Parser<Alts, unit> =
    (galts palt |>> PrimitiveAlts) <|> (galts aalt |>> AlgebraicAlts)

//Program: prog --> binds
//let prog = binds

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
