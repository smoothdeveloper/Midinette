# Midinette

A small Parisian MIDI library: naïve and sentimental about the standard, light on its feet across platforms.

Midinette is a set of F# MIDI tools. It covers messages, sysex, registers and device-specific bits for Elektron and Waldorf gear. A cross-platform port backend is built on [midir](https://github.com/Boddlnagg/midir).

## Layout

| Path | What |
| --- | --- |
| `src/midinette` | Core: `MidiMessage`, sysex helpers, `Midi.ByteStream` raw-byte parser, `IMidiPlatform` / `IVirtualMidiPlatform` (`netstandard2.0;net10.0`) |
| `src/midinette.platform.midir` | Port backend on midir via a small Rust cdylib (`net10.0`) |
| `native/midinette-midir-ffi` | The Rust C-ABI shim over midir 0.11 |
| `src/midinette.elektron`, `src/midinette.waldorf` | Device specifics |
| `samples/MidirMonitor` | Lists ports, creates "Midinette In/Out" virtual ports, echoes and prints |
| `tests/*` | Expecto test projects, run with `dotnet test` |

## Midir backend

```fsharp
open Midi
open midinette.platform
open midinette.platform.midir

let midi = MidirPlatform("my app")              // client name shown by the OS
for p in midi.InputPorts do printfn "%s" p.Name // re-enumerated on each access (hot-plug)

// open a hardware port by name
match midi.TryFindOutput "Elektron MachineDrum" with
| Some port ->
    use out = midi.GetOutput port
    out.Open()
    out.WriteMessage(MidiMessage.Encode 0x90uy 60uy 100uy)
    out.WriteSysex [| 0xF0uy; 0x7Euy; 0x7Fuy; 0x06uy; 0x01uy; 0xF7uy |] // identity request
| None -> ()

// virtual ports (macOS CoreMIDI, Linux ALSA; not Windows)
if midi.SupportsVirtualPorts then
    use vin = midi.CreateVirtualInput "Midinette In"   // already open
    let input = vin :> IMidiInput<int64>
    use _ = input.ChannelMessageReceived.Subscribe(fun e -> printfn "%O" e.Message)
    use _ = input.SysexReceived.Subscribe(fun s -> printfn "sysex %d bytes" (UMX.untag_sysex s).Length)
    ()
```

Behaviour:

- Events are raised on a per-input dispatcher thread, never on midir's thread. Once `Close()` / `Dispose()` returns, no more events are raised.
- Sysex comes in reassembled (up to 16 MiB by default). Realtime bytes interleaved in a sysex are delivered separately. Running status is expanded.
- `Close` / `Dispose` are idempotent. Opening a port that disappeared raises `MidirException`.
- On macOS, sysex of 64 KB or more is sent in 16 KB packets, because CoreMIDI packets top out at 65535 bytes.
- `IMidiOutput` timestamps are ignored: messages are sent immediately.
- Virtual ports: `SupportsVirtualPorts` is `true` on macOS and Linux/ALSA. On Windows it is `false` and `CreateVirtual*` throws `PlatformNotSupportedException`. Use [loopMIDI](https://www.tobias-erichsen.de/software/loopmidi.html) or Windows MIDI Services loopback endpoints there.

## Build and test

Requirements: the .NET 10 SDK and a Rust toolchain (`cargo`). Linux also needs `libasound2-dev`.

```sh
dotnet test Midinette.slnx                    # builds the Rust shim via cargo, then runs every Expecto project
dotnet run --project samples/MidirMonitor -- --seconds 30
```

- The Midir project runs `cargo build --release` on `native/midinette-midir-ffi` before building, then copies the native library next to the assembly.
- To use a prebuilt shim, pass `-p:MidinetteSkipCargo=true` and drop `libmidinette_midir.{dylib,so}` / `midinette_midir.dll` into `native/midinette-midir-ffi/target/release/`.
- Virtual-port tests (loopback, ≥64 KB sysex, 1000× open/close soak) only run where virtual ports work: macOS, and Linux with `/dev/snd/seq`. Elsewhere they are skipped.
- Test projects are plain executables too: `dotnet run --project tests/midinette.platform.midir.tests -- --summary`.

Packages are managed centrally in `Directory.Packages.props`. Paket and FAKE are gone.

## Legacy

`src/midinette.platform.rtmidi`, `src/midinette.platform.portmidi` and `src/midinette.fable` are kept for reference but are no longer in `Midinette.slnx` or maintained. They still reference the old paket setup. New code should use `midinette.platform.midir`.
