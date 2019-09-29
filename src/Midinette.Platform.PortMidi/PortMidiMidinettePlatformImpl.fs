namespace Midinette.Platform.PortMidi
open Midi
open Midinette.Platform


module Impl =
    let toMidiEvent (e :PortMidi.MidiEvent) = MidiEvent<int>(Midi.MidiMessage.FromWord e.Message.Word, e.Timestamp)
    //let toMidiMessage   (m: PortMidi.MidiMessage) = Midi.MidiMessage.FromWord m.Word
    let fromMidiMessage (m: Midi.MidiMessage)     = PortMidi.MidiMessage.FromWord m.Word
    let toTaggedSysexEvent (h:IEvent<Handler<byte array>, byte array>) : IEvent<Handler<sysex_data>, sysex_data> = unbox h
        
open Impl

type PortMidiDeviceInfo = { deviceInfo: PortMidi.MidiDeviceInfo }
with
    interface IDeviceInfo with
        member d.Name = d.deviceInfo.Name
        //member d.DeviceId = d.deviceInfo.DeviceId
        
type PortMidiIn =
    { inPort: PortMidi.MidiInput }
with
    interface IMidiInput<int> with
        [<CLIEvent>] member x.Error = x.inPort.Error
        [<CLIEvent>] member x.ChannelMessageReceived  = x.inPort.ChannelMessageReceived  |> Event.map toMidiEvent            
        [<CLIEvent>] member x.SystemMessageReceived   = x.inPort.SystemMessageReceived   |> Event.map toMidiEvent        
        [<CLIEvent>] member x.SysexReceived           = x.inPort.SysexReceived |> Impl.toTaggedSysexEvent
        [<CLIEvent>] member x.RealtimeMessageReceived = x.inPort.RealtimeMessageReceived |> Event.map toMidiEvent          
        member x.Open bufferSize = x.inPort.Open bufferSize
        member x.Close () = x.inPort.Close ()
        member x.DeviceInfo = { deviceInfo = x.inPort.DeviceInfo } :> _

type PortMidiOut =
    { outPort: PortMidi.MidiOutput }
with
    interface IMidiOutput<int> with
        member x.Open bufferSize latency = x.outPort.Open bufferSize latency
        member x.Close () = x.outPort.Close ()
        member x.WriteMessage timestamp message = x.outPort.WriteMessage timestamp (fromMidiMessage message)
        member x.WriteMessages timestamp messages = x.outPort.WriteMessages timestamp (messages |> Array.map fromMidiMessage)
        member x.WriteSysex timestamp sysex = x.outPort.WriteSysex timestamp (UMX.untag_sysex sysex)
        member x.DeviceInfo = { deviceInfo = x.outPort.DeviceInfo } :> _
                                  
 
type PortMidiMidinettePlatformImpl (platform: PortMidi.MidiPlatformTrigger) =
    let platform = platform
    
    new () = PortMidiMidinettePlatformImpl(PortMidi.Runtime.platform)
    
    member x.Events = x :> IMidiPlatformEvents<_>
    member x.Platform = x :> IMidiPlatform<_>
    
    interface IMidiPlatformEvents<int> with
        [<CLIEvent>] member x.Error                   = platform.Error                   |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, e)
        [<CLIEvent>] member x.ChannelMessageReceived  = platform.ChannelMessageReceived  |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, toMidiEvent e)
        [<CLIEvent>] member x.SystemMessageReceived   = platform.SystemMessageReceived   |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, toMidiEvent e)
        [<CLIEvent>] member x.SysexReceived           = platform.SysexReceived           |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, e)
        [<CLIEvent>] member x.RealtimeMessageReceived = platform.RealtimeMessageReceived |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, toMidiEvent e)
    interface IMidiPlatform<int> with        
        member x.GetMidiInput (device: IDeviceInfo) =
            match device with
            | :? PortMidiDeviceInfo as pmDevice -> Some ({ inPort = PortMidi.MidiInput(pmDevice.deviceInfo, PortMidi.Runtime.ptGetTime)} :> IMidiInput<_>)
            | _ -> None 
        member x.GetMidiOutput device =
            match device with
            | :? PortMidiDeviceInfo as pmDevice -> Some ({ outPort = PortMidi.MidiOutput(pmDevice.deviceInfo, PortMidi.Runtime.ptGetTime)} :> IMidiOutput<_>)
            | _ -> None
        member x.InputDevices =
            PortMidi.Runtime.getDevices ()
            |> Array.filter (fun d -> d.SupportsInput)
            |> Array.map (fun d -> { deviceInfo = d } :> IDeviceInfo)
        member x.OutputDevices =
            PortMidi.Runtime.getDevices ()
            |> Array.filter (fun d -> d.SupportsOutput)
            |> Array.map (fun d -> { deviceInfo = d } :> IDeviceInfo)
            