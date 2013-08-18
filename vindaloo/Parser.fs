module Vindaloo.Parser

open FParsec
open Vindaloo.Syntax

let ws = spaces
let str = pstring
let list p =
    between (str "{") (str "}") (
       ((sepBy (p .>> ws) (str "," .>> ws))) <|>
       (ws >>. preturn(List.empty))
    )
let ``->`` = ws .>> str "->" .>> ws

//Literals: literal --> 0# | 1# | ...
let literal : Parser<Literal, unit> = pint32 .>> str "#"

//Primitive ops: prim --> +# | -# | *# | /#
let prim : Parser<Operator, unit> =
    (str "+#" >>% (+)) <|>
    (str "-#" >>% (-)) <|>
    (str "*#" >>% (*)) <|>
    (str "/#" >>% (/))

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
//forward declare it here so it can be used in patterns
let expr, exprImpl = createParserForwardedToRef()

//Update flag: pi --> u | n
let pi : Parser<Updateable, unit> =
    (str "\\u" >>% true) <|>
    (str "\\n" >>% false)

//Lambda-forms: lf --> vars(f) \pi vars(a) -> expr
let lf : Parser<LambdaForm, unit> =
   pipe4 vars (ws >>. pi .>> ws) vars (``->`` >>. expr) (
      fun fv up pa bd -> { freeVars=fv; updateable=up; parameters=pa; body=bd }
   )

//Bindings: binds --> var(1) = lf(1); ... ; var(n) = lf(n)    n >= 1
let binding : Parser<Binding, unit> = var .>> ws .>> str "=" .>> ws .>>. lf .>> ws
   //(pipe2 (binding .>>? str ";") (sepBy binding (str ";")) List.Cons) |> Map.ofList
let binds : Parser<Bindings, unit> = sepBy1 binding (str ";" .>> ws) |>> Map.ofList

let exprBinds : Parser<Bindings * Expr, unit> = 
    binds .>> ws .>> str "in" .>> ws .>>. expr

let letBinds : Parser<Expr, unit> =
    str "let" >>. exprBinds |>>
    fun (binds, expr) -> LetE {binds = binds; expr = expr}
    
let letrec : Parser<Expr, unit> =
    str "letrec" >>. exprBinds |>>
    fun (binds, expr) -> LetrecE {binds = binds; expr = expr}

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
let galts xalt = sepEndBy xalt (str ";" .>> ws) .>> ws .>>. dalt
                 |>> fun (alts, def) -> {cases = Map.ofList alts; def = def}
let alts : Parser<Alts, unit> =
    (galts palt |>> PrimitiveAlts) <|> (galts aalt |>> AlgebraicAlts)

//case
let case : Parser<Expr, unit> =
    str "case" >>. ws >>. expr .>> ws .>> str "of" .>> ws .>>. alts |>>
    fun (expr, alts) -> CaseE {expr = expr; alts = alts}

//expr is implemented here
do exprImpl :=
    (letBinds) <|>
    (letrec) <|>
    (case) <|>
    (appl) <|>
    (constrAppl |>> ConstrApplE) <|>
    (primAppl) <|>
    (literal |>> LiteralE)

//Program: prog --> binds
let prog = ws >>. binds .>> ws

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
