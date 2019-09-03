namespace Midinette.Platform

type IDeviceInfo =
  abstract member Name     : string
  //abstract member DeviceId : int

type IRawInputPort<'rawdata> =
  [<CLIEvent>] abstract member DataReceived : IEvent<'rawdata>

type IRawOutputPort<'rawdata> =   
  abstract member WriteMessage: midiMessage: 'rawdata -> unit
  
type IMidiInput<'midievent> =
  [<CLIEvent>] abstract member Error : IEvent<string>
  [<CLIEvent>] abstract member ChannelMessageReceived : IEvent<'midievent>
  [<CLIEvent>] abstract member SystemMessageReceived : IEvent<'midievent>
  [<CLIEvent>] abstract member SysexReceived : IEvent<byte array>
  [<CLIEvent>] abstract member RealtimeMessageReceived : IEvent<'midievent>
  abstract member Open: bufferSize:int -> unit
  abstract member Close: unit -> unit
  abstract member DeviceInfo : IDeviceInfo

type IMidiOutput<'timestamp,'midimessage> =
  abstract member WriteMessage:  timestamp:'timestamp -> midiMessage:'midimessage        -> unit
  abstract member WriteMessages: timestamp:'timestamp -> midiMessages:'midimessage array -> unit
  abstract member WriteSysex:    timestamp:'timestamp -> data:byte array                 -> unit
  abstract member Open:          bufferSize:int       -> latency: int                    -> unit
  abstract member Close:         unit                                                    -> unit
  abstract member DeviceInfo : IDeviceInfo


type IMidiPlatformEvents<'device, 'event, 'timestamp, 'midimessage> =
  [<CLIEvent>] abstract member Error : IEvent<'device * string>
  [<CLIEvent>] abstract member ChannelMessageReceived : IEvent<'device * 'event>
  [<CLIEvent>] abstract member SystemMessageReceived : IEvent<'device * 'event>
  [<CLIEvent>] abstract member SysexReceived : IEvent<'device * byte array>
  [<CLIEvent>] abstract member RealtimeMessageReceived : IEvent<'device * 'event>

type IMidiPlatform<'device, 'event, 'timestamp, 'midimessage> =
  abstract member GetMidiInput: device: 'device -> IMidiInput<'event> option
  abstract member GetMidiOutput: device: 'device -> IMidiOutput<'timestamp, 'midimessage> option
  abstract member InputDevices: 'device array
  abstract member OutputDevices: 'device array

type MidiPlatformTrigger<'device, 'event, 'timestamp, 'midimessage>() =
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
  interface IMidiPlatformEvents<'device, 'event, 'timestamp, 'midimessage> with
    [<CLIEvent>] member x.Error = error.Publish
    [<CLIEvent>] member x.ChannelMessageReceived  = channelMessageReceived.Publish
    [<CLIEvent>] member x.SystemMessageReceived   = systemMessageReceived.Publish
    [<CLIEvent>] member x.SysexReceived           = sysexReceived.Publish
    [<CLIEvent>] member x.RealtimeMessageReceived = realtimeMessageReceived.Publish

