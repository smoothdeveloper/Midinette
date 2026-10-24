module Fable.Midinette.Fable.Tests

open Fable.Core
open Fable.Core.JsInterop
//open Fable.Midinette.Fable
open midinette.Fable
open Fable.Core.Testing

[<Global>]
let it (_msg: string) (_f: unit->unit): unit = jsNative
let add a b = a + b
it "Adding works" <| fun () ->
    let expected = 3
    let actual = add 1 2
    Assert.AreEqual(expected,actual)
