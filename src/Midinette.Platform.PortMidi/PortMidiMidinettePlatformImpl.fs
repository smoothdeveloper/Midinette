namespace Midinette.Platform.PortMidi
open Midi
open Midinette.Platform


module internal Impl =
    let toMidiEvent (e :PortMidi.MidiEvent) = MidiEvent<int>(Midi.MidiMessage.FromWord e.Message.Word, e.Timestamp)
    //let toMidiMessage   (m: PortMidi.MidiMessage) = Midi.MidiMessage.FromWord m.Word
    let fromMidiMessage (m: Midi.MidiMessage)     = PortMidi.MidiMessage.FromWord m.Word
    
open Impl
type PortMidiIn =
    { inPort: PortMidi.MidiInput }
with
    interface IMidiInput<MidiEvent<int>> with
        [<CLIEvent>] member x.Error = x.inPort.Error
        [<CLIEvent>] member x.ChannelMessageReceived  = x.inPort.ChannelMessageReceived  |> Event.map toMidiEvent            
        [<CLIEvent>] member x.SystemMessageReceived   = x.inPort.SystemMessageReceived   |> Event.map toMidiEvent        
        [<CLIEvent>] member x.SysexReceived           = x.inPort.SysexReceived
        [<CLIEvent>] member x.RealtimeMessageReceived = x.inPort.RealtimeMessageReceived |> Event.map toMidiEvent          
        member x.Open bufferSize = x.inPort.Open bufferSize
        member x.Close () = x.inPort.Close ()
        member x.DeviceInfo = { new IDeviceInfo with 
                                      member d.Name = x.inPort.DeviceInfo.Name
                                      member d.DeviceId = x.inPort.DeviceInfo.DeviceId }

type PortMidiOut =
    { outPort: PortMidi.MidiOutput }
with
    interface IMidiOutput<int,Midi.MidiMessage> with
        member x.Open bufferSize latency = x.outPort.Open bufferSize latency
        member x.Close () = x.outPort.Close ()
        member x.WriteMessage timestamp message = x.outPort.WriteMessage timestamp (fromMidiMessage message)
        member x.WriteMessages timestamp messages = x.outPort.WriteMessages timestamp (messages |> Array.map fromMidiMessage)
        member x.WriteSysex timestamp sysex = x.outPort.WriteSysex timestamp sysex
        member x.DeviceInfo = { new IDeviceInfo with 
                                      member d.Name = x.outPort.DeviceInfo.Name
                                      member d.DeviceId = x.outPort.DeviceInfo.DeviceId }

type PortMidiDeviceInfo = { deviceInfo: PortMidi.MidiDeviceInfo }
with
    interface IDeviceInfo with
        member d.Name = d.deviceInfo.Name
        member d.DeviceId = d.deviceInfo.DeviceId 
type PortMidiMidinettePlatformImpl (platform: PortMidi.MidiPlatformTrigger) =
    let platform = platform
    
    new () = PortMidiMidinettePlatformImpl(PortMidi.Runtime.platform)
    interface IMidiPlatformEvents<IDeviceInfo,Midi.MidiEvent<int>,int,Midi.MidiMessage> with
        [<CLIEvent>] member x.Error                   = platform.Error                   |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, e)
        [<CLIEvent>] member x.ChannelMessageReceived  = platform.ChannelMessageReceived  |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, toMidiEvent e)
        [<CLIEvent>] member x.SystemMessageReceived   = platform.SystemMessageReceived   |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, toMidiEvent e)
        [<CLIEvent>] member x.SysexReceived           = platform.SysexReceived           |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, e)
        [<CLIEvent>] member x.RealtimeMessageReceived = platform.RealtimeMessageReceived |> Event.map (fun (d,e) -> {deviceInfo = d } :> IDeviceInfo, toMidiEvent e)
    interface IMidiPlatform<IDeviceInfo,Midi.MidiEvent<int>,int,Midi.MidiMessage> with        
        member x.GetMidiInput device =
            match device with
            | :? PortMidiDeviceInfo as pmDevice -> Some ({ inPort = PortMidi.MidiInput(pmDevice.deviceInfo, PortMidi.Runtime.ptGetTime)} :> IMidiInput<_>)
            | _ -> None 
        member x.GetMidiOutput device =
            match device with
            | :? PortMidiDeviceInfo as pmDevice -> Some ({ outPort = PortMidi.MidiOutput(pmDevice.deviceInfo, PortMidi.Runtime.ptGetTime)} :> IMidiOutput<_,_>)
            | _ -> None
        member x.InputDevices =
            PortMidi.Runtime.getDevices ()
            |> Array.filter (fun d -> d.SupportsInput)
            |> Array.map (fun d -> { deviceInfo = d } :> IDeviceInfo)
        member x.OutputDevices =
            PortMidi.Runtime.getDevices ()
            |> Array.filter (fun d -> d.SupportsOutput)
            |> Array.map (fun d -> { deviceInfo = d } :> IDeviceInfo)
            