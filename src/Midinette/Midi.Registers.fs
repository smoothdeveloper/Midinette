module Midi.Registers

open Midi
open System.Collections.Generic

type MidiChannelState =
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

  member this.UpdateWithEvent (m: MidiEvent) =
    let message = m.Message
    let channelState = this
    match message.MessageType with
    | MidiMessageType.ProgramChange    -> channelState.program <- message.Data1
    | MidiMessageType.ChannelPressure  -> channelState.channelPressure <- message.Data1
    | MidiMessageType.ControllerChange -> channelState.NoticeControllerChange message.Data1 message.Data2
    | MidiMessageType.PitchBendChange  -> channelState.pitchBend <- ((int16 message.Data2) <<< 7) ||| (int16 message.Data1)
    | MidiMessageType.NoteOn           -> channelState.NoticeNoteOn message.Data1 message.Data2 m.Timestamp
    | MidiMessageType.NoteOff          -> channelState.NoticeNoteOff message.Data1 message.Data2 m.Timestamp
    | _ -> ()


type ISysexInputState =
  abstract member BeginSysex      : unit -> unit
  abstract member DisposeSysex    : unit -> unit
  abstract member SysexInProgress : bool
  abstract member WriteSysexByte  : byte -> unit
  abstract member SysexData       : byte array
 


type IMidiPlatform<'device, 'event> =
  //abstract member GetMidiInput: device: 'device -> IMidiInput option
  //abstract member GetMidiOutput: device: 'device -> IMidiOutput option
  [<CLIEvent>] abstract member Error : IEvent<'device * string>
  [<CLIEvent>] abstract member ChannelMessageReceived : IEvent<'device * MidiEvent>
  [<CLIEvent>] abstract member SystemMessageReceived : IEvent<'device * MidiEvent>
  [<CLIEvent>] abstract member SysexReceived : IEvent<'device * byte array>
  [<CLIEvent>] abstract member RealtimeMessageReceived : IEvent<'device * 'event>

type MidiPlatformTrigger<'device, 'event>() =
  let error                   = new Event<_>()
  let realtimeMessageReceived = new Event<_>()
  let sysexReceived           = new Event<_>()
  let channelMessageReceived  = new Event<_>()
  let systemMessageReceived   = new Event<_>()
  member x.NoticeError(d,m) = error.Trigger(d,m)
  member x.NoticeRealtimeMessage(d, m) = realtimeMessageReceived.Trigger (d,m)
  member x.NoticeSysex(d, m) = sysexReceived.Trigger (d,m)
  member x.NoticeSystemMessage(d, m) = systemMessageReceived.Trigger(d,m)
  member x.NoticeChannelMessage(d, m) = channelMessageReceived.Trigger(d,m)
  interface IMidiPlatform<'device, 'event> with
    //member x.GetMidiInput d = getMidiInput d
    //member x.GetMidiOutput d = getMidiOutput d
    [<CLIEvent>] member x.Error = error.Publish
    [<CLIEvent>] member x.ChannelMessageReceived  = channelMessageReceived.Publish
    [<CLIEvent>] member x.SystemMessageReceived   = systemMessageReceived.Publish
    [<CLIEvent>] member x.SysexReceived           = sysexReceived.Publish
    [<CLIEvent>] member x.RealtimeMessageReceived = realtimeMessageReceived.Publish

open Midi.MidiMessageTypeIdentifaction


module PlatformImplHelp =

  let internal getMessageType message : MidiMessageType =
    if message <= 14 then (message &&& 240) else (message &&& 255)
    |> byte
    |> LanguagePrimitives.EnumOfValue 

  let internal completeSysex deviceInfo (sysexState: ISysexInputState) (platform: MidiPlatformTrigger<_,_>)=
    if sysexState.SysexData.Length > 5 then
      (deviceInfo, sysexState.SysexData) |> platform.NoticeSysex
    sysexState.DisposeSysex ()

  let internal processSysexMessage (deviceInfo) (sysexInput: ISysexInputState) message (platform: MidiPlatformTrigger<_,_>)=
    let mutable endEncountered = false
    for i in 0 .. 3 do
      if not endEncountered then
        let b = (message >>> (i * 8)) &&& 255 |> byte
        if b < 128uy || b = 247uy then
          sysexInput.WriteSysexByte b
        if b = 247uy then
          completeSysex deviceInfo sysexInput platform
          endEncountered <- true
  
  let processEvents (device: 'device) (events: 'event array) getDeviceInfo (platform: MidiPlatformTrigger<_,_>) makeMidiEvent getMessageWord (getSysexInputState: 'device -> ISysexInputState) =
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
  
  (*
type BaseMidiPlatform<'device>(hasHostError, getHostErrorText, getDeviceInfo) =

  let error = new Event<_>()
  //let inputDeviceGate = obj()
  //let inputDevices = Dictionary<int,'device (*OpenedDevice*)>()
  let processMidiEvents device events =
    if hasHostError device then
      error.Trigger (getDeviceInfo device (*d.Device*), getHostErrorText ())
    
      (*
      match Platform.Pm_Poll(d.Stream) with
      | PmError.GotData -> 
        match read d.Stream readBufferSize with
        | Choice2Of2 message -> Error.Trigger(d.Device, message)
        | Choice1Of2 events ->
          processEvents d events
      | PmError.NoData -> ()
      | err ->
        error.Trigger(d.Device, getErrorText err)
        *)
  interface IMidiPlatform<'device> with
    [<CLIEvent>] member x.Error = error.Publish
    //[<CLIEvent>] member x.ChannelMessageReceived = channelMessageReceived
    //[<CLIEvent>] member x.SystemMessageReceived = systemMessageReceived
    //[<CLIEvent>] member x.SysexReceived = sysexReceived
    //[<CLIEvent>] member x.RealtimeMessageReceived = realtimeMessageReceived    *)