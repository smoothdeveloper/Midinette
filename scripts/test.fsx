#load ".paket/load/net472/RtMidi.Core.fsx"
#r @"C:\dev\src\gitlab.com\gauthier\Midinette\build\Debug\AnyCPU\netstandard2.0\Midinette.dll"
#r @"C:\dev\src\gitlab.com\gauthier\Midinette\build\Debug\AnyCPU\netstandard2.0\Midinette.Platform.RtMidi.dll"
open Midinette.Platform
let platform = Midinette.Platform.RtMidi.RtMidiMidinettePlatformImpl()

let iplatform = platform :> IMidiPlatform<IDeviceInfo, Midi.MidiEvent<int64>, int64, Midi.MidiMessage>
iplatform.InputDevices |> printfn "%A"

let mdm = global.RtMidi.Core.MidiDeviceManager.Default
mdm.GetAvailableMidiApis()
mdm.InputDevices
mdm.OutputDevices