//namespace vindaloo.Tests

module Vindaloo.Test.Parser

open FParsec
open Xunit
open FsUnit.Xunit
open Vindaloo.Parser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result; true
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg; false

//literal
[<Fact>]
let ``primitve`` () = test literal "2#" |> should equal true
[<Fact>]
let ``wrong primitive`` () = test literal "2" |> should equal false

//primitive operation
[<Fact>]
let ``primitve op`` () = test prim "-#" |> should equal true
[<Fact>]
let ``wrong primitive op`` () = test prim "2+" |> should equal false

//variable
[<Fact>]
let ``identifier`` () = test var "asdfs" |> should equal true
[<Fact>]
let ``wrong identifier`` () = test var "1" |> should equal false

//list of vars
[<Fact>]
let ``list of vars`` () = test vars "{asdfs,  asdf,sdf,sdf}" |> should equal true
[<Fact>]
let ``empty list of vars`` () = test vars "{}" |> should equal true
[<Fact>]
let ``a list of vars which was not closed`` () = test vars "{asdf, asdf" |> should equal false

//atom
[<Fact>]
let ``identifier - atom`` () = test var "asdfs" |> should equal true
[<Fact>]
let ``wrong identifier - atom`` () = test var "1" |> should equal false
[<Fact>]
let ``primitve op - atom`` () = test prim "-#" |> should equal true
[<Fact>]
let ``wrong primitive op - atom`` () = test prim "2+" |> should equal false


//matching
//default alts
[<Fact>]
let ``testestest`` () = test expr "13#" |> should equal true
[<Fact>]
let ``default alternative without var`` () = test dalt "default -> 13#" |> should equal true
[<Fact>]
let ``default alternative with var`` () = test dalt "a -> 3#" |> should equal true
[<Fact>]
let ``default alternative is not a constructor`` () = test dalt "A {} -> 3#" |> should equal false
[<Fact>]
let ``default alternative has no params`` () = test dalt "a {4#} -> 7#" |> should equal false
[<Fact>]
let ``default alternative is not a primitive`` () = test dalt "4# -> 7#" |> should equal false
//primitive alts
[<Fact>]
let ``primitive alt`` () = test palt "3# -> 13#" |> should equal true
[<Fact>]
let ``another primitive alt`` () = test palt "7# -> 13#" |> should equal true
[<Fact>]
let ``yet another primitive alt`` () = test palt "1337# -> 13#" |> should equal true
[<Fact>]
let ``primitive alternative is not default`` () = test palt "default -> 13#" |> should equal false
[<Fact>]
let ``primitive alternative is not a var`` () = test palt "a -> 3#" |> should equal false
[<Fact>]
let ``primitive alternative is not a constructor`` () = test palt "A {3#} -> 3#" |> should equal false
[<Fact>]
let ``primitive alternative has no params`` () = test palt "a {4#} -> 7#" |> should equal false
//algebraic alts
[<Fact>]
let ``algebraic alt`` () = test aalt "A {} -> 13#" |> should equal true
[<Fact>]
let ``another algebraic alt`` () = test aalt "A {7#} -> 13#" |> should equal true
[<Fact>]
let ``yet another algebraic alt`` () = test aalt "A {b, c} -> 13#" |> should equal true
[<Fact>]
let ``algebraic alternative is not default`` () = test aalt "default -> 13#" |> should equal false
[<Fact>]
let ``algebraic alternative is not a var`` () = test aalt "a -> 3#" |> should equal false
[<Fact>]
let ``algebraic alternative is not a function`` () = test aalt "a {4#} -> 7#" |> should equal false
//alt lists
[<Fact>]
let ``algebraic alts`` () = test (galts aalt) "A {} -> 13#; default -> 13#" |> should equal true
[<Fact>]
let ``more algebraic alts`` () = test (galts aalt) "A {} -> 13#;B {7#} -> 3#;C {a, b} -> 3#; default -> 13#" |> should equal true
[<Fact>]
let ``more algebraic alts with var default`` () = test (galts aalt) "A {} -> 13#; B {7#} -> 3#; C {a, b} -> 3#; a -> 13#" |> should equal true
[<Fact>]
let ``primitive alts`` () = test (galts palt) "3# -> 3#; default -> 13#" |> should equal true
[<Fact>]
let ``more primitive alts`` () = test (galts palt) "1# -> 13#; 7# -> 3#; 5# -> 3#; default -> 13#" |> should equal true
[<Fact>]
let ``more primitive alts with var default`` () = test (galts palt) "1# -> 13#;7# -> 3#;5# -> 3#; a -> 13#" |> should equal true
[<Fact>]
let ``alt lists need default`` () = test (galts palt) "1# -> 13#; 7# -> 3#; 5# -> 3#" |> should equal false
[<Fact>]
let ``alt lists need default even when only one option`` () = test (galts palt) "1# -> 13#" |> should equal false
[<Fact>]
let ``only default is also an alt list`` () = test (galts palt) "a -> 13#" |> should equal true
//pi
[<Fact>]
let ``not updateable`` () = test pi "\\n" |> should equal true
[<Fact>]
let ``updateable`` () = test pi "\\u" |> should equal true

//lf
[<Fact>]
let ``empty lf`` () = test lf "" |> should equal false
[<Fact>]
let ``lf with no parameters, updateable`` () = test lf "{} \\u {} -> 1#" |> should equal true
[<Fact>]
let ``lf with one parameter, updateable`` () = test lf "{asdf} \\u {asdf} -> 1#" |> should equal true
[<Fact>]
let ``lf with two parameter, non-updateable`` () = test lf "{b,d} \\n {c,f} -> 1#" |> should equal true

//binds
[<Fact>]
let ``no binding`` () = test binds "" |> should equal false
[<Fact>]
let ``one binding`` () = test binds "ab = {} \\u {} -> 1#" |> should equal true
[<Fact>]
let ``two bindings`` () = test binds "ab = {} \\u {} -> 1# ; ab = {} \\u {} -> 1#" |> should equal true



(* Test Input for Vindaloo *)

(*
  add = \a b ->
     case a of {
         a -> case b of {
             b -> primOp + a b
         }
     }
*)

[<Fact>]
let ``add`` () =
    test binds """
       add = \a b ->
         case a of {
           a -> case b of {
               b -> primOp + a b
           }
       }
    """ |> should equal true

(*
  compose = \f g x ->
     let gx = g x
     in f gx
*)

[<Fact>]
let ``compose`` () =
    test binds """
       compose = \f g x ->
          let gx = g x
          in f gx
    """ |> should equal true

(*
    map = \f xs->
        case xs of {
            Cons x xs ->
              let fx = f x
              in let mapfxs = map f xs
                 in Cons fx mapfxs
            ; Nil -> Nil
        }
*)

[<Fact>]
let ``map`` () =
    test binds """
map = {} \n {f,xs} ->
    case xs of
        Nil {} -> Nil {};
        Cons {y,ys} -> let fy = {f,y} \u {} -> f {y};
                           mfy = {f,ys} \u {} -> map {f,ys}
                       in Cons {fy,mfy}
    """ |> should equal true

[<Fact>]
let ``map1`` () =
    test binds """
map1 = {} \n {f} ->
       letrec
         mf = {f,mf} \n {xs} ->
              case xs of
                Nil {} -> Nil {}
                Cons {y,ys} -> let fy = {f,y} \u {} -> f {y}
                                   mfy = {mf,ys} \u {} -> mf {ys}
                               in Cons {fy,mfy}
        in mf
    """ |> should equal true