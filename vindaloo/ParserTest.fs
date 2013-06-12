//namespace vindaloo.Tests

module Vindaloo.Test.Parser

open Xunit
open FsUnit.Xunit
open Vindaloo.Parser

[<Fact>]
let ``one is one`` () = one |> should equal 1