//namespace vindaloo.Tests

module Vindaloo.Test.Parser

open FParsec
open Xunit
open FsUnit.Xunit
open Vindaloo.Parser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let one = 1

[<Fact>]
let ``one is one`` () = one |> should equal 1

[<Fact>]
let ``two is two`` () = 2 |> should equal 2

[<Fact>]
let ``bla`` () = one |> should equal 1

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