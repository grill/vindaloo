// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

// Define your library scripting code here
(*
#r "vindaloo"

module Vindaloo

open FParsec

printfn "wat"

ParserTest.test binds """
map1 = {} \n {f} ->
       letrec
         mf = {f,mf} \n {xs} ->
              case xs {} of
                Nil {} -> Nil {};
                Cons {y,ys} -> let fy = {f,y} \u {} -> f {y};
                                   mfy = {mf,ys} \u {} -> mf {ys}
                               in Cons {fy,mfy};
                default -> Nil {}
        in mf {}
    """ |> printfn
Threading.Thread.Sleep -1
*)