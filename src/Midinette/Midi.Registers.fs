module Midi.Registers
open Midinette.Platform
open Midi
open System.Collections.Generic
type MidiRealtimeState =
    val mutable started : bool
    new () = {started = false}
    member this.UpdateWithEvent (midiMessage: MidiMessage) =
      match midiMessage.MessageType with
      | MidiMessageType.Start ->
        this.started <- true
      | MidiMessageType.Stop ->
        this.started <- false
      | _ -> ()
   // member this.started = started

type MidiChannelState<'timestamp> =
  val seenControllers         : HashSet<byte>
  val controllers             : byte array
  val activeNotes             : HashSet<byte>
  val notes                   : (byte * int) array
  //val mutable modWheel        : byte
  val mutable pitchBend       : int16
  val mutable channelPressure : byte
  val mutable program         : byte
  
  new() =
    {
      program         = 0uy
      controllers     = Array.zeroCreate 128
      seenControllers = HashSet<_>()
      activeNotes     = HashSet<_>()
      notes           = Array.zeroCreate 128
      pitchBend       = 8192s
      channelPressure = 0uy
    }
  member this.NoticeProgramChange program =
    this.program <- program
  member this.NoticeControllerChange cc value =
    this.seenControllers.Add cc |> ignore
    this.controllers.[int cc] <- value
  member this.NoticeNoteOn note velocity timestamp =
    if velocity = 0uy then this.NoticeNoteOff note velocity timestamp else
    this.activeNotes.Add note |> ignore
    this.notes.[int note] <- velocity, timestamp
  member this.NoticeNoteOff note velocity timestamp =
    this.activeNotes.Remove note |> ignore
    this.notes.[int note] <- velocity, timestamp

  member this.UpdateWithEvent (m: MidiEvent<_>) =
    let message = m.Message
    match message.MessageType with
    | MidiMessageType.ProgramChange    -> this.program <- message.Data1
    | MidiMessageType.ChannelPressure  -> this.channelPressure <- message.Data1
    | MidiMessageType.ControllerChange -> this.NoticeControllerChange message.Data1 message.Data2
    | MidiMessageType.PitchBendChange  -> this.pitchBend <- ((int16 message.Data2) <<< 7) ||| (int16 message.Data1)
    | MidiMessageType.NoteOn           -> this.NoticeNoteOn message.Data1 message.Data2 m.Timestamp
    | MidiMessageType.NoteOff          -> this.NoticeNoteOff message.Data1 message.Data2 m.Timestamp
    | _ -> ()


type ISysexInputState =
  abstract member BeginSysex      : unit -> unit
  abstract member DisposeSysex    : unit -> unit
  abstract member SysexInProgress : bool
  abstract member WriteSysexByte  : byte -> unit
  abstract member SysexData       : byte array
 




module PlatformImplHelp =
  open Midi.MidiMessageTypeIdentifaction
  let internal getMessageType message : MidiMessageType =
    if message <= 14 then (message &&& 240) else (message &&& 255)
    |> byte
    |> LanguagePrimitives.EnumOfValue 

  let internal completeSysex deviceInfo (sysexState: ISysexInputState) (platform: MidiPlatformTrigger<_,_,_,_>)=
    if sysexState.SysexData.Length > 5 then
      (deviceInfo, sysexState.SysexData) |> platform.NoticeSysex
    sysexState.DisposeSysex ()

  let internal processSysexMessage (deviceInfo) (sysexInput: ISysexInputState) message (platform: MidiPlatformTrigger<_,_,_,_>)=
    let mutable endEncountered = false
    for i in 0 .. 3 do
      if not endEncountered then
        let b = (message >>> (i * 8)) &&& 255 |> byte
        if b < 128uy || b = 247uy then
          sysexInput.WriteSysexByte b
        if b = 247uy then
          completeSysex deviceInfo sysexInput platform
          endEncountered <- true
  
  let processEvents (device: 'device) (events: 'event array) getDeviceInfo (platform: MidiPlatformTrigger<_,_,_,_>) makeMidiEvent getMessageWord (getSysexInputState: 'device -> ISysexInputState) =
    let deviceInfo = getDeviceInfo device
    let sysexInputState : ISysexInputState = getSysexInputState device
  
    for e in events do
      let word : int = getMessageWord e
      let messageType = getMessageType word
      let midiEvent = makeMidiEvent e
      if isRealtimeMessage messageType then
        (deviceInfo, midiEvent) |> platform.NoticeRealtimeMessage
      elif sysexInputState.SysexInProgress && messageType <> MidiMessageType.SysEx then
        processSysexMessage deviceInfo sysexInputState word platform
      elif isSystemMessage messageType then
        if messageType = MidiMessageType.SysEx then
          if sysexInputState.SysexInProgress then
            // accomodate for incomplete sysex
            completeSysex deviceInfo sysexInputState platform
          sysexInputState.BeginSysex()
          processSysexMessage deviceInfo sysexInputState word platform
        else
          (deviceInfo, midiEvent) |> platform.NoticeSystemMessage
      else
        (deviceInfo, midiEvent) |> platform.NoticeChannelMessage
  