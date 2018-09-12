module Midinette.Fable
open Midi
module WM = Fable.Import.WebMIDI

type DeviceInfo(port: WM.IMIDIPort) =
  member x.Port = port
  interface Midi.IDeviceInfo with
    member x.DeviceId = port.Id |> int
    member x.Name = sprintf "id:%20A name:%20A manufacturer:%A" port.Id port.Name.Value port.Manufacturer

type MyMidiIn(input: WM.IMIDIInput, onMidiIn, onStateChange) =
  let midiMessage = new Event<_>()
  let stateMessage = new Event<_>()
  do

    input.set_OnMidiMessage (fun mm ->
      onMidiIn mm
      midiMessage.Trigger mm
    )
    input.set_OnStateChange (fun sc ->
      onStateChange sc
      stateMessage.Trigger sc
    )
  [<CLIEvent>] member x.MidiMessage = midiMessage.Publish
  [<CLIEvent>] member x.StateMessage = stateMessage.Publish

type WebMIDIInputPortWrapper(webMidiInput: WM.IMIDIInput, onStateChange, onMidiIn) as this = 
  let sysexReceived           = new Event<_>()
  let systemMessageReceived   = new Event<_>()
  let channelMessageReceived  = new Event<_>()
  let error                   = new Event<_>()
  let realtimeMessageReceived = new Event<_>()

  do
    let midiIn = this :> Midi.IMidiInput<double>

    webMidiInput.set_OnMidiMessage (fun mm ->
      let midiMessage = Midi.MidiMessage.Encode mm.Data.[0] mm.Data.[1] mm.Data.[2]

      if midiMessage.IsChannelMessage then
        channelMessageReceived.Trigger(MidiEvent(midiMessage, mm.ReceivedTime))
      match onMidiIn with
      | Some onMidiIn -> onMidiIn mm
      | _ -> ()
      
    )
    webMidiInput.set_OnStateChange (fun sc ->
      match onStateChange with
      | Some onStateChange -> onStateChange sc
      | _ -> ()
    )
  
  interface Midi.IMidiInput<double> with
    member x.DeviceInfo = DeviceInfo(webMidiInput) :> _
    member x.Close () = webMidiInput.Close () |> ignore
    member x.Open bufferSize = webMidiInput.Open() |> ignore
    [<CLIEvent>] member x.Error                   = error.Publish
    [<CLIEvent>] member x.ChannelMessageReceived  = channelMessageReceived.Publish
    [<CLIEvent>] member x.SystemMessageReceived   = systemMessageReceived.Publish
    [<CLIEvent>] member x.SysexReceived           = sysexReceived.Publish
    [<CLIEvent>] member x.RealtimeMessageReceived = realtimeMessageReceived.Publish


type WebMIDIOutputPortWrapper(webMidiOutput: WM.IMIDIOutput, device, onStateChange) as this =
    do
        let midiOut = this :> Midi.IMidiOutput<double>
        
        webMidiOutput.set_OnStateChange (fun sc ->
            match onStateChange with
            | Some onStateChange -> onStateChange sc
            | _ -> ()
        )

    interface IMidiOutput<double> with
        member x.Open bufferSize latency = ()
        member x.Close () = ()
        member x.WriteMessage timestamp message =
            webMidiOutput.SendAt (float timestamp) [|message.Status; message.Data1; message.Data2|]
        member x.WriteMessages timestamp messages =
            messages |> Array.iter (((x :> IMidiOutput<_>).WriteMessage )timestamp)
        member x.WriteSysex timestamp data =
            webMidiOutput.SendAt (float timestamp) data
        member x.DeviceInfo = (DeviceInfo device) :> _

  
(*
open Fable.Core

(************************************************)
(*     You should remove the next lines and     *)
(*    start writing your library in this file   *)
(************************************************)

[<Emit("$0 + $1")>]
let add (x: int) (y: int) = jsNative

*)
