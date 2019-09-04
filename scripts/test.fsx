#load "../.paket/load/net472/fsnative.fsx"
#load "../.paket/load/net472/RtMidi.Core.fsx"
#r @"C:\dev\src\gitlab.com\gauthier\Midinette\build\Debug\AnyCPU\netstandard2.0\Midinette.dll"
#r @"C:\dev\src\gitlab.com\gauthier\Midinette\build\Debug\AnyCPU\netstandard2.0\Midinette.Platform.RtMidi.dll"
open RtMidi.Core.Devices

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
let iplatform = platform :> IMidiPlatform<IDeviceInfo, Midi.MidiEvent<int64>, int64, Midi.MidiMessage>

let monoOut =
  (iplatform.InputDevices |> Array.filter (fun d -> d.Name = "monomachine-out ")).[0]
  |> iplatform.GetMidiInput

monoOut.Value.ChannelMessageReceived.Add(fun m -> printfn "[%20i] channel message: %A" m.Timestamp m.Message)
monoOut.Value.SystemMessageReceived.Add(fun m -> printfn "[%20i] system message: %A" m.Timestamp m.Message)
monoOut.Value.RealtimeMessageReceived.Add(fun m -> printfn "[%20i] realtime message: %A" m.Timestamp m.Message)
monoOut.Value.SysexReceived.Add(fun bytes -> printfn "sysex message: %A" bytes)
monoOut.Value.Open (1024*1024)
let mdm = global.RtMidi.Core.MidiDeviceManager.Default
mdm.GetAvailableMidiApis()

let rtmin = (mdm.InputDevices |> Seq.find (fun d -> d.Name = "monomachine-out ")).CreateDevice()
              
rtmin.InputDevice.Message.Add(fun bytes -> printfn "inputmessage %A" bytes)
rtmin.add_Nrpn (NrpnMessageHandler(fun a n -> printfn "[%20i] nrpn %A %i %i" platform.Now n.Channel n.Parameter n.Value))
rtmin.Open()
mdm.OutputDevices



let e : RtMidi.Core.Unmanaged.Devices.RtMidiInputDevice = null
