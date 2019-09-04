namespace Midinette.Platform.RtMidi

open System.Reflection
open Midinette.Platform

module Impl =
    let toBytes (message: Midi.MidiMessage) = [|message.Status;message.Data1;message.Data2|]
    let rtMidiOut (bytes: byte array) port =
        let f = port.GetType().GetField("_outputDevice", BindingFlags.NonPublic ||| BindingFlags.Instance)
        let rtoutport = f.GetValue port
        let sendmethod = rtoutport.GetType().GetMethod "SendMessage"
        let paramters = box bytes
        let result = sendmethod.Invoke(rtoutport, Array.singleton paramters) :?> bool        
        ()
        
open Impl
open Midi
open RtMidi.Core.Unmanaged.Devices
open System.Diagnostics
    
type InPortEvents<'timestamp> = {
  error : Event<string>  
  channelMessage : Event<Midi.MidiEvent<'timestamp>>
  systemMessage: Event<Midi.MidiEvent<'timestamp>>
  sysex: Event<byte array>
  realtimeMessage: Event<Midi.MidiEvent<'timestamp>>
}

type RtMidiInfo = { info: RtMidi.Core.Devices.Infos.IMidiDeviceInfo }
with
    interface IDeviceInfo with 
        member d.Name = d.info.Name

type RtMidiIn = { inPort: RtMidi.Core.Devices.IMidiInputDevice; info: RtMidiInfo; inEvents: InPortEvents<int64> }
with
    interface IMidiInput<Midi.MidiEvent<int64>> with
        
        member x.Close () = x.inPort.Close()
        member x.Open bufferSize = x.inPort.Open() |> ignore
        member x.DeviceInfo = x.info :> _
        [<CLIEvent>] member x.Error = x.inEvents.error.Publish
        [<CLIEvent>] member x.ChannelMessageReceived = x.inEvents.channelMessage.Publish
        [<CLIEvent>] member x.SystemMessageReceived = x.inEvents.systemMessage.Publish
        [<CLIEvent>] member x.SysexReceived = x.inEvents.sysex.Publish
        [<CLIEvent>] member x.RealtimeMessageReceived = x.inEvents.realtimeMessage.Publish
    
type RtMidiOut<'timestamp> = { outPort: RtMidi.Core.Devices.IMidiOutputDevice; info: RtMidiInfo }
with
    interface IMidiOutput<'timestamp,Midi.MidiMessage> with
        member x.DeviceInfo = x.info :> _
        member x.Open bufferSize latency = x.outPort.Open() |> ignore
        member x.Close () = x.outPort.Close()
        member x.WriteSysex timestamp data = rtMidiOut data x.outPort
        member x.WriteMessage timestamp message = rtMidiOut (toBytes message) x.outPort 
        member x.WriteMessages timestamp messages =
            let bytes =
                Array.init
                    (messages.Length * 3)
                    (fun i ->
                        let message = messages.[i / 3]
                        match i % 3 with
                        | 0 -> message.Status
                        | 1 -> message.Data1
                        | 2 -> message.Data2
                        | _ -> failwithf "index %i not expected???" i
                    )
            rtMidiOut bytes x.outPort 

type RtMidiMidinettePlatformImpl() =
    let rtmidi = RtMidi.Core.MidiDeviceManager.Default
    let watch = Stopwatch.StartNew()
    let platformEvents = MidiPlatformTrigger()
    let platformEventsPublish = platformEvents :> IMidiPlatformEvents<_,_,_,_>
    
    member x.Trigger = platformEvents
    member x.Platform = x :> IMidiPlatformEvents<_,_,_,_>
    
    member x.Now = watch.ElapsedTicks
    interface IMidiPlatformEvents<IDeviceInfo, Midi.MidiEvent<int64>, int64, Midi.MidiMessage> with
        [<CLIEvent>] member x.Error                   = platformEventsPublish.Error                   |> Event.map (fun (d,e) -> {info = d } :> IDeviceInfo, e)
        [<CLIEvent>] member x.ChannelMessageReceived  = platformEventsPublish.ChannelMessageReceived  |> Event.map (fun (d,e) -> {info = d } :> IDeviceInfo, e)
        [<CLIEvent>] member x.SystemMessageReceived   = platformEventsPublish.SystemMessageReceived   |> Event.map (fun (d,e) -> {info = d } :> IDeviceInfo, e)
        [<CLIEvent>] member x.SysexReceived           = platformEventsPublish.SysexReceived           |> Event.map (fun (d,e) -> {info = d } :> IDeviceInfo, e)
        [<CLIEvent>] member x.RealtimeMessageReceived = platformEventsPublish.RealtimeMessageReceived |> Event.map (fun (d,e) -> {info = d } :> IDeviceInfo, e)
        
    
    interface IMidiPlatform<IDeviceInfo, Midi.MidiEvent<int64>, int64, Midi.MidiMessage> with
        member x.InputDevices =
            RtMidi.Core.MidiDeviceManager.Default.InputDevices
            |> Seq.map (fun d -> { info = d} :> IDeviceInfo)
            |> Seq.toArray
        member x.OutputDevices =
            RtMidi.Core.MidiDeviceManager.Default.InputDevices
            |> Seq.map (fun d -> { info = d} :> IDeviceInfo)
            |> Seq.toArray
            
        member x.GetMidiInput deviceInfo =
            match deviceInfo with
            | :? RtMidiInfo as rtDevice ->
                match rtDevice.info with
                | :? RtMidi.Core.Devices.Infos.IMidiInputDeviceInfo as device ->
                    // todo: keep that in store instead of creating / subscribing again
                    let port = device.CreateDevice()
                    let error = new Event<_>()
                    let channelMessage = new Event<_>()
                    let sysex = new Event<_>()
                    let systemMessage = new Event<_>()
                    let realtimeMessage = new Event<_>()
                    port.InputDevice.Message.Add(fun bytes ->
                      let status : MidiMessageType = LanguagePrimitives.EnumOfValue (bytes.[0])
                      let inline toMidiEvent (bytes: byte array) (watch: Stopwatch) =
                        let timestamp = watch.ElapsedTicks
                        let message = MidiMessage.Encode bytes.[0] bytes.[1] bytes.[2]
                        Midi.MidiEvent(message, timestamp)
                      
                      let inline triggerMessageEvent (event1: Event<_>) platformNoticer =
                        let event = toMidiEvent bytes watch 
                        event1.Trigger event
                        platformNoticer (device,event)
                      if MidiMessageTypeIdentifaction.isChannelMessage status then
                        triggerMessageEvent channelMessage platformEvents.NoticeChannelMessage
                      elif MidiMessageTypeIdentifaction.isRealtimeMessage status then
                        triggerMessageEvent realtimeMessage platformEvents.NoticeRealtimeMessage
                      elif MidiMessageTypeIdentifaction.isSystemMessage status then
                        if MidiMessageTypeIdentifaction.isSysexBeginOrEnd status then
                            sysex.Trigger bytes
                            platformEvents.NoticeSysex(device, bytes)
                        else
                            triggerMessageEvent systemMessage platformEvents.NoticeSystemMessage
                      else
                        #if DEBUG
                        failwithf "unable to parse what I received received %A" bytes
                        #endif
                    )
                    let inEvents = {error = error; channelMessage = channelMessage; sysex = sysex; systemMessage = systemMessage; realtimeMessage = realtimeMessage }
                    Some ({ inPort = port; info = rtDevice; inEvents = inEvents } :> _)
                | _ -> None
            | _ -> None
        member x.GetMidiOutput deviceInfo =
            match deviceInfo with
            | :? RtMidiInfo as rtDevice ->
                match rtDevice.info with
                | :? RtMidi.Core.Devices.Infos.IMidiOutputDeviceInfo as device ->
                    Some ({ outPort = device.CreateDevice() ; info = rtDevice} :> IMidiOutput<_,_>)
                | _ -> None
            | _ -> None
            
