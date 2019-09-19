
namespace Midinette.Sysex
open Midi
module Sysex =
  open System
  open System.Diagnostics
  open Midinette.Platform
  let sysexDeviceInquiry =
    [|
    0xf0uy
    0x7euy
    0x7fuy
    0x06uy
    0x01uy
    0xf7uy
    |]

  let helpGetSysex maxMessage (timeout: TimeSpan) sysexIsMatching buildResponse (inPort: IMidiInput<_>) =
    let mutable response = None
    let mutable count = 0
    let rec noticeSysex (sysex: sysex_data) =
      if Option.isNone response then
        if sysexIsMatching sysex then
          response <- Some (buildResponse sysex)
        else
          count <- count + 1
    let subscription = inPort.SysexReceived.Subscribe(noticeSysex)
    async {
      let sw = new Stopwatch()
      sw.Start()
      while count < maxMessage && Option.isNone response && sw.Elapsed < timeout do
        do! Async.Sleep 5
      subscription.Dispose()
      return response
    }
    |> Async.Catch

  let getSysexMessagesFromBytes data =
    let getSlice  s l (a: _ array) = Array.init l (fun i -> a.[s + i])
    [|
      let mutable beginIndex = 0
      for i in 0 .. (Array.length data) - 1 do
        if data.[i] = 0xf0uy then beginIndex <- i
        elif data.[i] = 0xf7uy then yield getSlice beginIndex (i - beginIndex + 1) data
    |] |> UMX.tag_sysex_data

  type DetectedDevice<'timestamp> =
    
    | DetectedDevice of responseData: sysex_data * deviceOutput: IMidiInput<'timestamp> * deviceInput: IMidiOutput<'timestamp>
    | Error of exn * deviceOutput: IMidiInput<'timestamp> * deviceInput: IMidiOutput<'timestamp>

  let deviceInquiry (inputPorts: IMidiInput<'timestamp> array) (outputPorts: IMidiOutput<'timestamp> array) sysexMatcher doWithOutput withInputAndOutput =
    // maybe buggy
    let detectedPairs = [|
      let sysexInputTimeout = System.TimeSpan.FromSeconds 1.0
      for o in outputPorts do
        let responses =
          [|
              for i in inputPorts do
                use scope = withInputAndOutput i o
                let buildResponse sysexData = DetectedDevice(sysexData, i, o)
                let response = helpGetSysex 10 sysexInputTimeout sysexMatcher buildResponse i
                yield i,o,response
          |]
        doWithOutput o
        yield 
          (responses 
          |> Array.map (fun (_,_,response) -> response) 
          |> Async.Parallel 
          |> Async.RunSynchronously 
          |> Array.zip (responses |> Array.map (fun (i,o,_) ->(i,o)))
          )
      |]
    detectedPairs
    |> Array.map (
      fun items ->
        items 
        |> Array.map (
          fun ((i,o), result) ->
          
            match result with
            | Choice1Of2(data) -> data
            | Choice2Of2 exn   -> Some (Error(exn, i, o))
        )
    )
    |> Array.concat
    |> Array.choose id