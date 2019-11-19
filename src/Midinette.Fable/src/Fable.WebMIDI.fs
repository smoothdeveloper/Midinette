// updated version of https://github.com/magicmonty/fable-import-webmidi
module Fable.WebMIDI

open Fable.Core
open Fable.Core.JS
open Browser
type MIDIOptions =
    | Sysex of bool

[<StringEnum>]
type MIDIPortType =
    | Input
    | Output

[<StringEnum>]
type MIDIPortDeviceState =
    | Disconnected
    | Connected

[<StringEnum>]
type MIDIPortConnectionState =
    | Open
    | Closed
    | Pending

type IMIDIPort = 
    [<Emit("$0.id")>]
    abstract Id: string with get
    [<Emit("$0.manufacturer")>]
    abstract Manufacturer: string option with get
    [<Emit("$0.name")>]
    abstract Name: string option with get
    [<Emit("$0.type")>]
    abstract Type: MIDIPortType with get
    [<Emit("$0.version")>]
    abstract Version: string option with get
    [<Emit("$0.state")>]
    abstract State: MIDIPortDeviceState with get
    [<Emit("$0.connection")>]
    abstract Connection: MIDIPortConnectionState with get
    [<Emit("$0.onstatechange=$1")>]
    abstract OnStateChange: (IMIDIConnectionEvent -> unit) with set
    [<Emit("$0.open")>]
    abstract Open: (unit -> Promise<IMIDIPort>)
    [<Emit("$0.close")>]
    abstract Close: (unit -> Promise<IMIDIPort>)

and IMIDIConnectionEvent = 
    inherit Browser.Types.EventType
    [<Emit("$0.port")>]
    abstract Port: IMIDIPort with get

type IMIDIMessageEvent =
    inherit Browser.Types.EventType
    [<Emit("$0.receivedTime")>]
    abstract member ReceivedTime: double with get
    [<Emit("$0.data")>]
    abstract member Data: byte array with get

type IMIDIInput =
    inherit IMIDIPort
    [<Emit("$0.onmidimessage=$1")>]
    abstract OnMidiMessage: (IMIDIMessageEvent -> unit) with set

type IMIDIInputMap = JS.Map<string, IMIDIInput>

type IMIDIOutput =
    inherit IMIDIPort
    [<Emit("$0.send")>]
    abstract Send: (byte array -> unit)
    [<Emit("$0.send($2, $1)")>]
    abstract SendAt: (float -> byte array -> unit)
    [<Emit("$0.clear")>]
    abstract Clear: (unit -> unit)

type IMIDIOutputMap = JS.Map<string, IMIDIOutput>

type IMIDIAccess =
    [<Emit("$0.inputs")>]
    abstract Inputs: IMIDIInputMap with get
    [<Emit("$0.outputs")>]
    abstract Outputs: IMIDIOutputMap with get
    [<Emit("$0.onstatechange=$1")>]
    abstract OnStateChange: (IMIDIConnectionEvent -> unit) with set
    [<Emit("$0.sysexEnabled")>]
    abstract SysexEnabled: bool with get, set

module internal Intern =

    [<Emit("navigator.requestMIDIAccess($0)")>]
    let requestAccess (options: obj) : Promise<IMIDIAccess> = jsNative

module Map =
    let toList (m: JS.Map<'a,'b>): ('a*'b) list =
        let mutable result : ('a*'b) list = []
        m.forEach (fun b a _ -> result <- (a, b) :: result)
        result |> List.ofSeq

module ArrayBuffer =
    let toArray (m: ArrayBuffer) : byte array =
        let c = JS.Constructors.Uint8ClampedArray.Create(m)
        let mutable result : byte list = []
        c.forEach (fun a _ _ -> result <- (byte a) :: result; true)
        result |> List.rev |> List.toArray

[<RequireQualifiedAccess>]
module MIDI =
    let requestAccess (options: MIDIOptions list) =
        Intern.requestAccess (JsInterop.keyValueList CaseRules.LowerFirst options)

    let send (output: IMIDIOutput) (data : byte array) =
        promise {
            output.Send data
        }
    let sendAt (output: IMIDIOutput) (sendTime: float) (data : byte array) =
        promise {
            data |> output.SendAt sendTime
        }

