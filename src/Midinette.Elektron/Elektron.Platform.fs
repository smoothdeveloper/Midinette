module Elektron.Platform
open Elektron
open Midi
open System
open System.Collections.Generic
open FSharp.UMX


module SysexBufferEdit =
  let getAt     (bytes: sysex_data) baseAddress a : byte_7bits   = UMX.to_byte_7bits bytes.[baseAddress + a]
  let setAt     (bytes: sysex_data) baseAddress a (v:byte_7bits) = bytes.[baseAddress + a] <- UMX.tag_sysex_data ((UMX.untag v) &&& 0b01111111uy)
  //let getString (bytes: sysex_data) baseAddress length
  let getDataSlice  s l (a: _ array) : byte_7bits array = Array.init l (fun i -> unbox a.[s + i])

  // review usage

  let getSlice  s l (a: _ array) = Array.init l (fun i -> a.[s + i])
  let setSlice  s l (a: _ array) (values: _ array) =
    for i in 0 .. (l - 1) do
      a.[s + i] <- values.[i]
type data = byte_7bits array
type bytes = byte array
module SysexHelper =
  let header =
    UMX.to_sysex_data [|
      0xf0uy
      0x00uy
      0x20uy
      0x3cuy
      0x02uy
      0x00uy
    |]
  let makeMachineDrumSysexMessage (message: data) : sysex_data =
    let message = UMX.to_sysex_data message
    let sysexEnd = UMX.to_sysex_data [|0xf7uy|] 
    (Array.concat [|header;message;sysexEnd|])

let twoDataToShort (msb: byte<_>) (lsb: byte<_>) = 
  let msb = uint16 msb
  let lsb = uint16 lsb
  (msb <<< 7) + lsb
let byteToData (bytes: byte array) : data =
    let encodeBytes (bytes: byte array) =
        let mutable prefixByte = 0uy 
        for i in 0 .. (Array.length bytes) - 1 do
            prefixByte <- prefixByte ||| ((bytes.[i] >>> 7) <<< (6 - i))
            bytes.[i] <- bytes.[i] &&& 0b01111111uy
        Array.concat [|Array.singleton prefixByte;bytes|]
   
    bytes
    |> unbox 
    |> Array.chunkBySize 7
    |> Array.collect encodeBytes
    |> unbox

let dataToByte (data: data) : byte array =
    let inline prefixMask b i =
        let mask = 1uy <<< (6 - i)
       
        if b &&& mask <> 0uy then 0b10000000uy else 0uy

    let decodeData (data: byte array) =
        let prefix = data.[0]
        Array.init (data.Length - 1) (fun i -> data.[i + 1] ||| prefixMask prefix i)

    data
    |> unbox 
    |> Array.chunkBySize 8
    |> Array.collect decodeData
    |> unbox 

let toBigEndian (v:int32) =
  #if FABLE_COMPILER
  failwith "toBigEndian not implemented"
  #else
  System.Net.IPAddress.HostToNetworkOrder v
  #endif
  

let fourBytesToBigEndianInt b = BitConverter.ToInt32 (b, 0) |> toBigEndian

// FABLE TODO
(*
let getMachineDrumDataSliceFromSysexMessage data = ArraySegment(data, 0x9, (Array.length data) - 14)
let getMonoMachineDataSliceFromSysexMessage data = ArraySegment(data, 0xa, (Array.length data) - 15)
*)

type ArraySegment<'a>(data: 'a array, s: int, l: int) = 
  // FABLE TODO!!!!!!!!!!!!!!!
  let a = ()
  interface IEnumerable<'a> with
    member x.GetEnumerator() = (data:> IEnumerable<_>).GetEnumerator()
  interface System.Collections.IEnumerable with
    member x.GetEnumerator() = (data:> System.Collections.IEnumerable).GetEnumerator()

let inline getMachineDrumDataSliceFromSysexMessage (data: sysex_data) : ArraySegment<byte_7bits> = ArraySegment(unbox data, 0x9, (Array.length data) - 14)
let inline getMonoMachineDataSliceFromSysexMessage (data: sysex_data) : ArraySegment<byte_7bits> = ArraySegment(unbox data, 0xa, (Array.length data) - 15)


let getCheckSumFromSysexMessage (data: sysex_data) =
  let msb = data.[data.Length - 5]
  let lsb = data.[data.Length - 4]
  twoDataToShort msb lsb
let getLengthFromSysexMessage (data: sysex_data) =
  let msb = data.[data.Length - 3] 
  let lsb = data.[data.Length - 2]
  twoDataToShort msb lsb

let checkSum (data: byte<_> seq) =
  let sum = 
    data 
    |> Seq.sumBy int
  sum &&& 0b11111111111111
// FABLE TODO

let areMachineDrumCheckSumAndLengthValid data =
  
  let checkSum = 
    getMachineDrumDataSliceFromSysexMessage data
    |> checkSum
  let length = (Array.length data) - 10
  let expectedCheckSum = int (getCheckSumFromSysexMessage data)
  let expectedLength = int (getLengthFromSysexMessage data)
  printfn "%i %i %i %i" length expectedLength checkSum expectedCheckSum
  length = expectedLength && checkSum = expectedCheckSum
  
let areMonoMachineCheckSumAndLengthValid (data: sysex_data) =
  
  let checkSum = 
    //getMonoMachineDataSliceFromSysexMessage data
    data.[0x0a .. data.Length - 6]
    |> checkSum
  let length = (Array.length data) - 10
  let expectedCheckSum = int (getCheckSumFromSysexMessage data)
  let expectedLength = int (getLengthFromSysexMessage data)
  printfn "%i %i %i %i" length expectedLength checkSum expectedCheckSum
  length = expectedLength && checkSum = expectedCheckSum
  
module SilverMachines =
  [<RequireQualifiedAccess>]
  type PatternBank =
  | A | B | C | D
  | E | F | G | H
  with
    static member FromByte (b: byte<_>) =
      match UMX.untag b with
      | 0x0uy -> A
      | 0x1uy -> B
      | 0x2uy -> C
      | 0x3uy -> D
      | 0x4uy -> E
      | 0x5uy -> F
      | 0x6uy -> G
      | 0x7uy -> H
      | v -> failwithf "unkown bank %i" v
    static member ToByte =
      function
      | A -> 0x0uy
      | B -> 0x1uy
      | C -> 0x2uy
      | D -> 0x3uy
      | E -> 0x4uy
      | F -> 0x5uy
      | G -> 0x6uy
      | H -> 0x7uy
  type PatternLocator = PatternLocator of PatternBank * pattern:byte_7bits
  with
    static member FromByte (b:byte<_>) =
      let b = UMX.untag b
      let bank = PatternBank.FromByte (b / 16uy)
      let pattern = UMX.tag_byte_7bits (b % 16uy)
      PatternLocator(bank, pattern)

  [<RequireQualifiedAccess>]
  type Output =
  | OutputA
  | OutputB
  | OutputC
  | OutputD
  | OutputE
  | OutputF
  | OutputMain
  with
    static member FromByte (b: byte_7bits) =
      match UMX.untag b with
      | 0x0uy -> OutputA
      | 0x1uy -> OutputB
      | 0x2uy -> OutputC
      | 0x3uy -> OutputD
      | 0x4uy -> OutputE
      | 0x5uy -> OutputF
      | 0x6uy -> OutputMain
      | v -> failwithf "invalid output %i" v
    static member ToByte output =
      UMX.tag_byte_7bits (
        match output with
        | OutputA    -> 0x0uy
        | OutputB    -> 0x1uy
        | OutputC    -> 0x2uy
        | OutputD    -> 0x3uy
        | OutputE    -> 0x4uy
        | OutputMain -> 0x6uy
        | OutputF    -> 0x5uy
      )