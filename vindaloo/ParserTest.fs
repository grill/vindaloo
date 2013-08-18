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
        map = \f xs->
          case xs of {
              Cons x xs ->
                let fx = f x
                in let mapfxs = map f xs
                   in Cons fx mapfxs
              ; Nil -> Nil
          }
    """ |> should equal true