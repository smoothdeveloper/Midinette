
namespace Midi
module Sysex =
  open System
  open System.Diagnostics
  open Midi
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
    let rec noticeSysex (sysex: byte array) =
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

  let getSysexMessagesFromBytes data =
    let getSlice  s l (a: _ array) = Array.init l (fun i -> a.[s + i])
    [|
      let mutable beginIndex = 0
      for i in 0 .. (Array.length data) - 1 do
        if data.[i] = 0xf0uy then beginIndex <- i
        elif data.[i] = 0xf7uy then yield getSlice beginIndex (i - beginIndex + 1) data
    |]

  type DetectedDevice<'timestamp> = DetectedDevice of responseData: byte array * deviceOutput: IMidiInput<'timestamp> * deviceInput: IMidiOutput<'timestamp>

  let deviceInquiry (inputPorts: IMidiInput<_> array) (outputPorts: IMidiOutput<_> array) sysexMatcher doWithOutput withInputAndOutput =
    let detectedPairs = [|
      let sysexInputTimeout = System.TimeSpan.FromSeconds 5.0
      for o in outputPorts do
        let responses =
          [|
              for i in inputPorts do
                use scope = withInputAndOutput i o
                let buildResponse sysexData = DetectedDevice(sysexData, i, o)
                let response = helpGetSysex 10 sysexInputTimeout sysexMatcher buildResponse i
                yield response
          |]
        doWithOutput o
        yield (responses |> Async.Parallel |> Async.RunSynchronously)
      |]
    detectedPairs 
    |> Array.map (Array.choose id)
    |> Array.collect id
