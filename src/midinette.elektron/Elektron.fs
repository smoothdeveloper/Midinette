namespace Elektron
(*
module Measures =
    type [<Measure>] bit7' = class end
    type [<Measure>] bit8' = class end
    
    let inline (++) (w : ^W when ^W : (static member IsMeasureAbbrev : ^tm * ^t -> unit)) (t : ^t) = (# "" t : ^tm #)
    let inline (--) (w : ^W when ^W : (static member IsMeasureAbbrev : ^tm * ^t -> unit)) (tm : ^tm) = (# "" tm : ^t #)

// use member constraints to statically establish a measure relationship between types
type UoM = | UoM 
with
    static member IsMeasureAbbrev(_ : byte<'Measure>, _ : byte) = ()
    //static member IsMeasureAbbrev(_ : string<'Measure>, _ : string) = ()
[<MeasureAnnotatedAbbreviation>]
type byte<[<Measure>] 'Measure> = byte

type b7 = byte<Measures.bit7'>
type b8 = byte<Measures.bit8'>*)