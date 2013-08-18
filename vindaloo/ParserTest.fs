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
let ``identifier`` () = test var "_Asdfs" |> should equal true
[<Fact>]
let ``wrong identifier`` () = test var "1" |> should equal false

//list of vars
[<Fact>]
let ``list of vars`` () = test vars "{_Asdfs,  asdf,Asdf,sdf}" |> should equal true
[<Fact>]
let ``empty list of vars`` () = test vars "{}" |> should equal true
[<Fact>]
let ``a list of vars which was not closed`` () = test vars "{asdf, asdf" |> should equal false

//atom
[<Fact>]
let ``identifier - atom`` () = test var "_Asdfs" |> should equal true
[<Fact>]
let ``wrong identifier - atom`` () = test var "1" |> should equal false
[<Fact>]
let ``primitve op - atom`` () = test prim "-#" |> should equal true
[<Fact>]
let ``wrong primitive op - atom`` () = test prim "2+" |> should equal false

(* Test Input for Vindaloo *)

(*
  add = \a b ->
     case a of {
         a -> case b of {
             b -> primOp + a b
         }
     }
*)


(*
  compose = \f g x ->
     let gx = g x
     in f gx
*)

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