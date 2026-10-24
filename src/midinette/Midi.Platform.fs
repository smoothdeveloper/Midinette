namespace midinette.platform
open Midi

type IDeviceInfo =
  abstract member Name     : string
  //abstract member DeviceId : int

type IRawInputPort<'rawdata> =
  [<CLIEvent>] abstract member DataReceived : IEvent<'rawdata>

type IRawOutputPort<'rawdata> =   
  abstract member WriteMessage: midiMessage: 'rawdata -> unit
  
type IMidiInput<'timestamp> =
  [<CLIEvent>] abstract member Error : IEvent<string>
  [<CLIEvent>] abstract member ChannelMessageReceived : IEvent<MidiEvent<'timestamp>>
  [<CLIEvent>] abstract member SystemMessageReceived : IEvent<MidiEvent<'timestamp>>
  [<CLIEvent>] abstract member SysexReceived : IEvent<sysex_data>
  [<CLIEvent>] abstract member RealtimeMessageReceived : IEvent<MidiEvent<'timestamp>>
  abstract member Open: bufferSize:int -> unit
  abstract member Close: unit -> unit
  abstract member DeviceInfo : IDeviceInfo

type IMidiOutput<'timestamp> =
  abstract member WriteMessage:  timestamp:'timestamp -> midiMessage: MidiMessage        -> unit
  abstract member WriteMessages: timestamp:'timestamp -> midiMessages: MidiMessage array -> unit
  abstract member WriteSysex:    timestamp:'timestamp -> data:sysex_data -> unit
  abstract member Open:          bufferSize:int       -> latency: int                    -> unit
  abstract member Close:         unit                                                    -> unit
  abstract member DeviceInfo : IDeviceInfo

type IMidiPlatformEvents<'timestamp> =
  [<CLIEvent>] abstract member Error : IEvent<IDeviceInfo * string>
  [<CLIEvent>] abstract member ChannelMessageReceived  : IEvent<IDeviceInfo * MidiEvent<'timestamp>>
  [<CLIEvent>] abstract member SystemMessageReceived   : IEvent<IDeviceInfo * MidiEvent<'timestamp>>
  [<CLIEvent>] abstract member SysexReceived           : IEvent<IDeviceInfo * byte array>
  [<CLIEvent>] abstract member RealtimeMessageReceived : IEvent<IDeviceInfo * MidiEvent<'timestamp>>

type IMidiPlatform<'timestamp> =
  abstract member GetMidiInput: device: IDeviceInfo -> IMidiInput<'timestamp> option
  abstract member GetMidiOutput: device: IDeviceInfo -> IMidiOutput<'timestamp> option
  abstract member InputDevices: IDeviceInfo array
  abstract member OutputDevices: IDeviceInfo array

type MidiPlatformTrigger<'timestamp>() =
  let error                   = new Event<_>()
  let realtimeMessageReceived = new Event<_>()
  let sysexReceived           = new Event<_>()
  let channelMessageReceived  = new Event<_>()
  let systemMessageReceived   = new Event<_>()
  member x.NoticeError(d,m)            = error.Trigger(d,m)
  member x.NoticeRealtimeMessage(d, m) = realtimeMessageReceived.Trigger (d,m)
  member x.NoticeSysex(d, m)           = sysexReceived.Trigger (d,m)
  member x.NoticeSystemMessage(d, m)   = systemMessageReceived.Trigger(d,m)
  member x.NoticeChannelMessage(d, m)  = channelMessageReceived.Trigger(d,m)
  interface IMidiPlatformEvents<'timestamp> with
    [<CLIEvent>] member x.Error = error.Publish
    [<CLIEvent>] member x.ChannelMessageReceived  = channelMessageReceived.Publish
    [<CLIEvent>] member x.SystemMessageReceived   = systemMessageReceived.Publish
    [<CLIEvent>] member x.SysexReceived           = sysexReceived.Publish
    [<CLIEvent>] member x.RealtimeMessageReceived = realtimeMessageReceived.Publish


/// Capability for platforms able to create virtual (software) ports that other
/// applications can see. Check `SupportsVirtualPorts` before calling the factories.
/// Returned ports are already open; `Close` (or `Dispose` when implemented) removes them.
type IVirtualMidiPlatform<'timestamp> =
  /// true when the OS/backend can create virtual ports (e.g. macOS CoreMIDI, Linux ALSA/JACK).
  abstract member SupportsVirtualPorts : bool
  /// Creates a virtual *input*: a destination other applications can write to, we receive.
  abstract member CreateVirtualInput  : name: string -> IMidiInput<'timestamp>
  /// Creates a virtual *output*: a source other applications can read from, we send.
  abstract member CreateVirtualOutput : name: string -> IMidiOutput<'timestamp>
