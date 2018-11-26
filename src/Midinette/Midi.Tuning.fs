module Midi.Tuning
open Midi

type MessageId =
  | DumpRequestId            = 0uy
  | DumpResponseId           = 1uy
  | SingleNoteTuningChangeId = 2uy

let makeSysex deviceId (messageId: MessageId) data =
  [|
    [|
      0xf0uy
      0x7euy
      deviceId
      0x08uy
      byte messageId
    |]
    data
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

type BulkTuningDumpResponse = 
  {
    name         : string
    programNumber: byte
    tunings      : NoteTuning array
  }

type TuningMessage =
| BulkTuningDumpRequest  of tuningProgramNumber: byte
| BulkTuningDump         of BulkTuningDumpResponse
| SingleNoteTuningChange of noteIndexAndTunings: (byte * NoteTuning) array
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
    match x with
    | BulkTuningDumpRequest programNumber  -> Some [|programNumber|]
    | BulkTuningDump _         -> Some [||]
    | SingleNoteTuningChange tunings -> 
      Some(
          [|
          for (noteIndex, tuning) in tunings do
            let centIncrement = 0.0061m
            let centsAsShort = int16 (tuning.cents / centIncrement)
            let centFirstByte = byte ((centsAsShort >>> 7) &&& 0b01111111s)
            let centSecondByte = byte (centsAsShort &&& 0b01111111s)
            yield! [|noteIndex; tuning.semiTone; centFirstByte; centSecondByte|]
          |]
      )
    | TuningProgramSelect _    -> None



  member x.Send (deviceId: byte, midiOutput: IMidiOutput<_>, nowTimestamp) =
    let sysex, messageId = x.SysexData, x.MessageId
    match sysex, messageId with
    | Some (data), Some (messageId) ->
      midiOutput.WriteSysex (nowTimestamp()) (makeSysex deviceId messageId data)
    | _ -> ()

    match x with
    | BulkTuningDumpRequest _ | BulkTuningDump _ | SingleNoteTuningChange _ -> ()
    | TuningProgramSelect _ ->
      //let rpn = 3
      //let messages = Midi.Nrpn.makeNRPN2 1uy 1uy 1uy
      //midiOutput.WriteMessages (nowTimestamp()) messages
      ()
      (*
type NoteTuningChange =
| NoFrequency
| Hertz of hertz: decimal
| NoteAndCents of NoteTuning
*)