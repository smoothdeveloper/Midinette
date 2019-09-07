module Midi.Tuning
open Midinette.Platform
// based on specs from midi.org
// additional spec: http://www.somascape.org/midi/tech/spec.html#usx7E0800
type MessageId =
  | DumpRequestId            = 0uy
  | DumpResponseId           = 1uy
  | SingleNoteTuningChangeId = 2uy

let makeSysex deviceId (messageId: MessageId) data =
  let requiresCheckSum = 
    if messageId = MessageId.DumpResponseId then true
    else false

  let bytesForChecksum = 
    [|
      [|
        0x7euy
        deviceId
        0x08uy
        byte messageId
      |]
      data
    |]
    |> Array.concat

  let checkSum =
    if requiresCheckSum then 
      let mutable sum = 0uy 
      for b in bytesForChecksum do
        sum <- sum ^^^ b
      [|sum|]
    else
      [||]

  [|
    [|
      0xf0uy
    |]
    bytesForChecksum
    checkSum
    [|
      0xf7uy
    |]
  |]
  |> Array.concat

type NoteTuning =
  {
    semiTone: byte
    cents   : decimal
  }

type NoteTuningChange =
| NoFrequency
//| Hertz of hertz: decimal
| NoteAndCents of NoteTuning

type BulkTuningDumpResponse = 
  {
    name         : string
    programNumber: byte
    tunings      : NoteTuningChange array
  }

type TuningMessage =
| BulkTuningDumpRequest  of tuningProgramNumber: byte
| BulkTuningDump         of BulkTuningDumpResponse
| SingleNoteTuningChange of noteIndexAndTunings: (byte * NoteTuningChange) array
| TuningProgramSelect    of program: byte * channel: byte // + do RPNs inc/dec
with
  member x.IsRealTime =
    match x with
    | TuningProgramSelect _ | SingleNoteTuningChange _ -> true
    | BulkTuningDumpRequest _
    | BulkTuningDump _
      -> false

  member x.MessageId =
    match x with
    | BulkTuningDumpRequest _  -> Some MessageId.DumpRequestId
    | BulkTuningDump _         -> Some MessageId.DumpResponseId
    | SingleNoteTuningChange _ -> Some MessageId.SingleNoteTuningChangeId
    | TuningProgramSelect _    -> None

  member x.SysexData =
    let centIncrement = 0.0061m
    let centsAsShorts cents = int16 (cents / centIncrement)
    let centFirstByte centsAsShort = byte ((centsAsShort >>> 7) &&& 0b01111111s)
    let centSecondByte centsAsShort = byte (centsAsShort &&& 0b01111111s)
    match x with
    | BulkTuningDumpRequest programNumber  -> Some [|programNumber|]
    | BulkTuningDump {name=name; programNumber=programNumber; tunings=tunings} -> 
      Some 
        [|
          yield programNumber
          yield! (System.Text.Encoding.ASCII.GetBytes (name.PadLeft 16))
          for tuning in tunings do
            match tuning with
            | NoFrequency -> yield! [|0x7fuy; 0x7fuy; 0x7fuy |]
            | NoteAndCents(tuning) ->
              let centsAsShort = centsAsShorts tuning.cents
              let centFirstByte = centFirstByte centsAsShort
              let centSecondByte = centSecondByte centsAsShort
              yield! [|tuning.semiTone; centFirstByte; centSecondByte|]
        |]
    | SingleNoteTuningChange tunings -> 
      Some(
          [|
          for (noteIndex, tuning) in tunings do
            match tuning with
            | NoFrequency -> yield! [|noteIndex; 0x7fuy; 0x7fuy; 0x7fuy |]
            | NoteAndCents(tuning) ->
              let centsAsShort = centsAsShorts tuning.cents
              let centFirstByte = centFirstByte centsAsShort
              let centSecondByte = centSecondByte centsAsShort
              yield! [|noteIndex; tuning.semiTone; centFirstByte; centSecondByte|]
          |]
      )
    | TuningProgramSelect _    -> None

  member x.MakeMTSSysex deviceId =
    let sysex, messageId = x.SysexData, x.MessageId
    match sysex, messageId with
    | Some data, Some messageId ->
      Some (makeSysex deviceId messageId data)
    | _ -> None
  
  member x.Send (deviceId: byte, midiOutput: IMidiOutput<_>, nowTimestamp) =
    match x.MakeMTSSysex deviceId with
    | Some sysex -> midiOutput.WriteSysex (nowTimestamp())  sysex
    | None -> ()

    match x with
    | BulkTuningDumpRequest _ | BulkTuningDump _ | SingleNoteTuningChange _ -> ()
    | TuningProgramSelect _ ->
      //let rpn = 3
      //let messages = Midi.Nrpn.makeNRPN2 1uy 1uy 1uy
      //midiOutput.WriteMessages (nowTimestamp()) messages
      ()
      (*

*)