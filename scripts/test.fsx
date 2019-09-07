
#load "../.paket/load/net472/fsnative.fsx"
#load "../.paket/load/net472/RtMidi.Core.fsx"
#r @"C:\dev\src\gitlab.com\gauthier\Midinette\build\Debug\AnyCPU\netstandard2.0\Midinette.dll"
#r @"C:\dev\src\gitlab.com\gauthier\Midinette\build\Debug\AnyCPU\netstandard2.0\Midinette.Elektron.dll"
#r @"C:\dev\src\gitlab.com\gauthier\Midinette\build\Debug\AnyCPU\netstandard2.0\Midinette.Platform.RtMidi.dll"
#load @"C:\dev\src\gitlab.com\gauthier\ElektronControl\ToolLib\PrettyPrint.fs"
open Elektron.MachineDrum
open Midi
open RtMidi.Core.Devices
open RtMidi.Core.Devices.Nrpn
open Elektron
open Elektron.MonoMachine
open System

open fsnative
open fsnative.Internals
let libNames = 
  match () with
  | Windows -> [| (if System.Environment.Is64BitProcess then "rtmidi" else "rtmidi32") |]
  | OSX     -> [|"rtmidi"|]
  | Linux   -> [|"rtmidi"|]

let libPaths =
  match () with
  | Windows -> [|@"C:\dev\src\github.com\micdah\RtMidi.Core\RtMidi.Core\bin\Debug\net45\"|]

let loader = LibraryLoader.withRuntimeLoader id
let library = LibraryLoader.tryLoadLibrary libNames libPaths loader
match library with
| None -> failwithf "couldn't load rtmidi!"
| Some library -> printfn "loaded rtmidi!"

open Midinette.Platform
let platform = Midinette.Platform.RtMidi.RtMidiMidinettePlatformImpl()
let iplatform = platform :> IMidiPlatform<int64>




















//////////////////// MD
let mdOut = (iplatform.InputDevices |> Array.filter (fun d -> d.Name = "machinedrum-out ")).[0] |> iplatform.GetMidiInput  |> Option.get
let mdIn = (iplatform.OutputDevices |> Array.filter (fun d -> d.Name = "machinedrum-in ")).[0] |> iplatform.GetMidiOutput |> Option.get  

mdOut.ChannelMessageReceived.Add(fun m -> printfn "[%20i] channel message: %A" m.Timestamp m.Message)
mdOut.SystemMessageReceived.Add(fun m -> printfn "[%20i] system message: %A" m.Timestamp m.Message)
mdOut.RealtimeMessageReceived.Add(fun m -> printfn "[%20i] realtime message: %A" m.Timestamp m.Message)
mdOut.SysexReceived.Add(fun bytes -> printfn "sysex message: %A" bytes)

mdOut.Open (1024*1024)
mdIn.Open (1024*1024) 0 

let md = new MachineDrum<_>(mdOut, mdIn, (fun () -> platform.Now))


Elektron.MachineDrum.mdDetection
    (fun () -> platform.Now)
    [|mdOut|]
    [|mdIn|]
    None
    (fun md ->
        printfn "inPort: %s" md.MidiInPort.DeviceInfo.Name
        printfn "outPort: %s" md.MidiOutPort.DeviceInfo.Name
    )


let mdListener = new MachineDrumEventListener<_>(md, (fun () -> platform.Now))




mdListener.Event.Add(fun e -> printfn "[%20i] MD EVENT: %A" e.Timestamp e.Message)










//////////////////// MONOMACHINE
let monoOut = (iplatform.InputDevices |> Array.filter (fun d -> d.Name = "monomachine-out ")).[0] |> iplatform.GetMidiInput  |> Option.get
let monoIn = (iplatform.OutputDevices |> Array.filter (fun d -> d.Name = "monomachine-in ")).[0] |> iplatform.GetMidiOutput |> Option.get  






monoOut.ChannelMessageReceived.Add(fun m -> printfn "[%20i] channel message: %A" m.Timestamp m.Message)
monoOut.SystemMessageReceived.Add(fun m -> printfn "[%20i] system message: %A" m.Timestamp m.Message)
monoOut.RealtimeMessageReceived.Add(fun m -> printfn "[%20i] realtime message: %A" m.Timestamp m.Message)
monoOut.SysexReceived.Add(fun bytes -> printfn "sysex message: %A" bytes)
monoOut.Open (1024*1024)
monoIn.Open (1024*1024) 0 
let getNow () = platform.Now
let monomachine = new MonoMachine.MonoMachine<_,_>(monoOut, monoIn, getNow)
let evtl = new MonoMachineEventListener<_>(getNow, monomachine)
evtl.Event.Add(fun e ->
  match e.Message with
  | Sysex data -> printfn "[%20i] mnm:" e.Timestamp; PrettyPrint.printBytes data
  | _ -> printfn "[%20i] mnm: %A" e.Timestamp e.Message
)

monomachine.CurrentGlobalSettings
monomachine.DumpPattern 0uy
let (Some(Kit(kit))) = monomachine.DumpKit 16uy
let (Some(Pattern(pattern))) = monomachine.DumpPattern 0uy
pattern
|> Elektron.Platform.dataToByte
kit.unpacked.[0xb..0xb+6] 





monomachine.MidiInPort.WriteMessage 0L (Midi.MidiMessage.NoteOn 0x00uy 0x64uy 100uy)
monomachine.MidiInPort.WriteMessage 0L (Midi.MidiMessage.NoteOff 0x00uy 0x64uy 100uy)
monomachine.DumpPattern 0uy
let globalsysex =
  """
f0 00 20 3c  03 00 50 03  01 00 12 08  00 02 06 07 
02 01 54 03  02 03 01 05  00 09 00 0a  0b 0c 0d 0e 
01 02 00 07  0a 01 02 07  0a 01 00 02  07 0a 01 02 
07 0a 00 01  02 07 0a 01  02 07 21 0a  06 01 00 64 
40 03 00 00  0b 17 23 2f  3b 47 0b 53  5f 6b 17 77 
40 7f 52 20  10 41 00 01  02 00 20 7f  05 00 11 30 
0e 30 00 5b  f7
""".Split([|' ';'\n'|], StringSplitOptions.RemoveEmptyEntries)
  |> Array.map (fun s -> Convert.ToByte(s, 16))

monomachine.DumpKit 0uy
let (MonoMachineSysexResponse.Kit(kit) ) = MonoMachineSysexResponse.BuildResponse globalsysex
kit.unpacked.Length

let gdata =
  ArraySegment(globalsysex, 0xa, (Array.length globalsysex) - 15)
  |> Seq.toArray
  |> Elektron.Platform.dataToByte
//  |> PrettyPrint.printBytes  
gdata.Length
PrettyPrint.printBytes (Elektron.Platform.dataToByte a)
PrettyPrint.printBytes globalsysex




let mdm = global.RtMidi.Core.MidiDeviceManager.Default
mdm.GetAvailableMidiApis()

let rtmin = (mdm.InputDevices |> Seq.find (fun d -> d.Name = "machinedrum-out ")).CreateDevice()
              
rtmin.InputDevice.Message.Add(fun bytes -> printfn "==========="; PrettyPrint.printBytes bytes)
//rtmin.add_Nrpn (NrpnMessageHandler(fun a n -> printfn "[%20i] nrpn %A %i %i" platform.Now n.Channel n.Parameter n.Value))

rtmin.Open()
mdm.OutputDevices

rtmin.SetNrpnMode NrpnMode.On



let e : RtMidi.Core.Unmanaged.Devices.RtMidiInputDevice = null
