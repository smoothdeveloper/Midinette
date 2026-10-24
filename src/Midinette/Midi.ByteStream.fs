/// Byte-stream MIDI 1.0 parser for backends that deliver raw bytes (midir, WebMIDI, serial...).
/// Handles running status, system realtime interleaved anywhere (incl. inside sysex),
/// sysex split over several chunks, and cancels sysex on unexpected status bytes.
module Midi.ByteStream
open Midi

[<RequireQualifiedAccess>]
type ParsedMidi =
  | Channel      of MidiMessage
  | SystemCommon of MidiMessage
  | Realtime     of MidiMessage
  /// complete sysex, F0 .. F7 inclusive
  | Sysex        of byte array
  /// bytes that could not be interpreted (orphan data, truncated/overlong sysex)
  | Dropped      of reason: string * bytes: byte array

/// number of data bytes following a status byte; -1 for sysex/undefined handling
let dataLength (status: byte) =
  if status < 0x80uy then -1
  elif status < 0xf0uy then
    match status &&& 0xf0uy with
    | 0xc0uy | 0xd0uy -> 1
    | _ -> 2
  else
    match status with
    | 0xf1uy | 0xf3uy -> 1
    | 0xf2uy -> 2
    | 0xf0uy -> -1
    | _ -> 0

/// wire length (status + data) of a packed `MidiMessage`
let messageLength (message: MidiMessage) =
  match dataLength message.Status with
  | -1 -> 1
  | n -> 1 + n

/// wire bytes of a packed `MidiMessage`
let toBytes (message: MidiMessage) =
  match messageLength message with
  | 1 -> [| message.Status |]
  | 2 -> [| message.Status; message.Data1 |]
  | _ -> [| message.Status; message.Data1; message.Data2 |]

let inline private isRealtime (b: byte) = b >= 0xf8uy

type MidiByteStreamParser(maxSysexLength: int) =
  let sysex = ResizeArray<byte>()
  let mutable inSysex = false
  let mutable runningStatus = 0uy
  let mutable expected = 0
  let data = Array.zeroCreate<byte> 2
  let mutable count = 0

  new() = MidiByteStreamParser(16 * 1024 * 1024)

  member _.MaxSysexLength = maxSysexLength
  member _.SysexInProgress = inSysex

  member _.Reset() =
    sysex.Clear()
    inSysex <- false
    runningStatus <- 0uy
    count <- 0

  member this.Feed(bytes: byte array, emit: ParsedMidi -> unit) =
    this.Feed(bytes, 0, bytes.Length, emit)

  member _.Feed(bytes: byte array, offset: int, length: int, emit: ParsedMidi -> unit) =
    let abortSysex reason =
      if inSysex then
        emit (ParsedMidi.Dropped(reason, sysex.ToArray()))
        sysex.Clear()
        inSysex <- false
    for i in offset .. offset + length - 1 do
      let b = bytes.[i]
      if isRealtime b then
        if b <> 0xf9uy && b <> 0xfduy then
          emit (ParsedMidi.Realtime(MidiMessage.Encode b 0uy 0uy))
      elif b = 0xf0uy then
        abortSysex "sysex interrupted by new sysex"
        inSysex <- true
        runningStatus <- 0uy
        sysex.Add b
      elif b = 0xf7uy then
        if inSysex then
          sysex.Add b
          emit (ParsedMidi.Sysex(sysex.ToArray()))
          sysex.Clear()
          inSysex <- false
        else
          emit (ParsedMidi.Dropped("orphan F7", [| b |]))
      elif b >= 0x80uy then
        abortSysex "sysex interrupted by status byte"
        let n = dataLength b
        if b >= 0xf0uy then
          // system common cancels running status
          runningStatus <- 0uy
          if n = 0 then
            emit (ParsedMidi.SystemCommon(MidiMessage.Encode b 0uy 0uy))
          else
            runningStatus <- b
            expected <- n
            count <- 0
        else
          runningStatus <- b
          expected <- n
          count <- 0
      else
        // data byte
        if inSysex then
          if sysex.Count >= maxSysexLength then
            abortSysex "sysex exceeds MaxSysexLength"
          else
            sysex.Add b
        elif runningStatus = 0uy then
          emit (ParsedMidi.Dropped("data byte without status", [| b |]))
        else
          data.[count] <- b
          count <- count + 1
          if count = expected then
            let m = MidiMessage.Encode runningStatus data.[0] (if expected = 2 then data.[1] else 0uy)
            if runningStatus >= 0xf0uy then
              emit (ParsedMidi.SystemCommon m)
              runningStatus <- 0uy
            else
              emit (ParsedMidi.Channel m)
            count <- 0
