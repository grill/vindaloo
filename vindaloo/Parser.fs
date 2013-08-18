module Vindaloo.Parser

open FParsec
open Vindaloo.Syntax

let ws = spaces
let str = pstring

//Literals: literal --> 0# | 1# | ...
let literal : Parser<uint64, unit> = puint64 .>> str "#"

//Primitive ops: prim --> +# | -# | *# | /#
let prim : Parser<Operator, unit> =
    (str "+#" >>. preturn(+)) <|>
    (str "-#" >>. preturn(-)) <|>
    (str "*#" >>. preturn(*)) <|>
    (str "/#" >>. preturn(/))

let isAsciiIdStart = fun c -> isAsciiLetter c || c = '_'
let var : Parser<string, unit> =
    identifier (IdentifierOptions(isAsciiIdStart = isAsciiIdStart))

//Variable lists: vars --> {var(1), ... , var(n)}    n >= 0
let vars : Parser<Vars, unit> = between (str "{") (str "}") (
       ((sepBy (var .>> ws) (str "," .>> ws))) <|>
       (ws >>. preturn(List.empty))
    )

//Atom: atom --> var | literal
//let atom = var <|> literal

//Atom lists: atoms --> {atom(1), ..., atom(n)}    n >= 0
//let atoms = between (str "{") (str "}") (sepBy atom (str ","))

//Bindings: binds --> var(1) = lf(1); ... ; var(n) = lf(n)    n >= 1
//let bind = var .>> ws .>> str "=" .>> ws .>>. lf .>> ws
//let binds = bind .>>? str ";" .>>. sepBy bind (str ";")

//Update flag: pi --> u | n
//let pi = str "u" <|> str "n"

//TODO: FIXME
//let constr = str "constr"

//Expression: expr --> let binds in expr
//                  | letrec binds in expr
//                  | case expr of alts
//                  | var atoms
//                  | constr atoms
//                  | prim atoms
//                  | literal
(*
let expr = (str "let" >>. binds .>> ws .>> str "in" .>> ws .>> expr) <|>
    (str "letrec" >>. binds .>> ws .>> str "in" .>> ws .>> expr) <|>
    (str "case" >>. expr .>> ws .>> str "of" .>> ws .>> alts) <|>
    (var .>> ws .>> atoms) <|>
    (constr >> ws >>. atoms) <|>
    (prim .>> ws .>>. atoms) <|>
    (literal)
*)

//let ``->`` = ws .>> str "->" .>> ws

//Lambda-forms: lf --> vars(f) \pi vars(a) -> expr
//let lf = vars .>> str "\\" .>>. pi .>>. vars .>> ``->`` .>>. expr

//Default alt: default --> var -> expr
//                      | default -> expr
//let dalt = (str "default" <|> var) .>> ``->`` .>>. expr

//Primitive alt: palt --> literal -> expr
//let palt = literal .>> ``->`` .>>. expr

//Algebraic alt: aalt --> constr vars -> expr
//let aalt = constr >>. vars .>> ``->`` .>>. expr

//Alternatives: alts --> aalt(1); ... ; aalt(n); default    n >= 0 (Algebraic)
//                   | palt(1); ... ; palt(n); default    n >= 0 (Primitive)
//let alts = (sepEndBy aalt (str ";")  <|> sepEndBy palt (str ";")) .>> ws .>>. dalt

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
