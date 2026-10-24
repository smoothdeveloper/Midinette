/// Tests against the real midir native lib. Virtual-port tests run where midir supports
/// them (macOS CoreMIDI, Linux ALSA with /dev/snd/seq) and are skipped elsewhere.
module Midinette.Platform.Midir.Tests.MidirTests
open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Threading
open Expecto
open Midi
open Midinette.Platform
open Midinette.Platform.Midir

/// tag a plain byte with whatever unit of measure the message constructors expect
let inline tb (b: byte) : FSharp.UMX.byte<'m> = FSharp.UMX.UMX.tag b

let platform = lazy (MidirPlatform("midinette-tests"))

let virtualPortsUsable () =
  let p = platform.Force()
  p.SupportsVirtualPorts
  && (OperatingSystem.IsMacOS() || (OperatingSystem.IsLinux() && File.Exists "/dev/snd/seq"))

let requireVirtual () =
  if not (virtualPortsUsable ()) then skiptest "virtual MIDI ports unavailable on this OS/runner"

let uniqueName prefix = sprintf "%s %s" prefix (Guid.NewGuid().ToString("N").Substring(0, 8))

let waitFor (timeout: TimeSpan) (f: unit -> 'a option) =
  let sw = Stopwatch.StartNew()
  let mutable r = f ()
  while r.IsNone && sw.Elapsed < timeout do
    Thread.Sleep 10
    r <- f ()
  r

let findInput name = waitFor (TimeSpan.FromSeconds 3.) (fun () -> platform.Force().TryFindInput name)
let findOutput name = waitFor (TimeSpan.FromSeconds 3.) (fun () -> platform.Force().TryFindOutput name)

/// collects everything an input raises
type Collector(input: IMidiInput<int64>) =
  let channel = ConcurrentQueue<MidiMessage>()
  let realtime = ConcurrentQueue<MidiMessage>()
  let sysex = ConcurrentQueue<byte array>()
  let errors = ConcurrentQueue<string>()
  let subs = [
    input.ChannelMessageReceived.Subscribe(fun e -> channel.Enqueue e.Message)
    input.RealtimeMessageReceived.Subscribe(fun e -> realtime.Enqueue e.Message)
    input.SysexReceived.Subscribe(fun s -> sysex.Enqueue(UMX.untag_sysex s))
    input.Error.Subscribe(errors.Enqueue) ]
  member _.Channel = channel
  member _.Realtime = realtime
  member _.Sysex = sysex
  member _.Errors = errors
  member _.Total = channel.Count + realtime.Count + sysex.Count
  interface IDisposable with
    member _.Dispose() = for s in subs do s.Dispose()

let waitCount (timeoutSeconds: float) (f: unit -> int) expected =
  waitFor (TimeSpan.FromSeconds timeoutSeconds) (fun () -> if f () >= expected then Some () else None) |> ignore
  f ()

let fixtureSysex () =
  let dir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "data", "machinedrum")
  if Directory.Exists dir then
    [ for f in Directory.GetFiles(dir, "*.syx") do
        yield! Midinette.Sysex.Sysex.getSysexMessagesFromBytes (File.ReadAllBytes f) |> Array.map UMX.untag_sysex ]
  else []

let bigSysex size =
  Array.init size (fun i -> if i = 0 then 0xf0uy elif i = size - 1 then 0xf7uy elif i = 1 then 0x7duy else byte (i % 128))

[<Tests>]
let native =
  testList "midir native" [
    test "abi + backend" {
      let p = platform.Force()
      let backend = p.BackendName
      Expect.isNotEmpty backend "backend name"
      if OperatingSystem.IsMacOS() then Expect.equal backend "coremidi" "macOS backend"
      if OperatingSystem.IsWindows() then Expect.isFalse p.SupportsVirtualPorts "WinMM has no virtual ports"
    }
    test "enumerate ports" {
      let p = platform.Force()
      if OperatingSystem.IsLinux() && not (File.Exists "/dev/snd/seq") then
        // no ALSA sequencer (containers, CI): midir cannot create a client; must fail cleanly
        let e = Expect.throwsC (fun () -> p.InputPorts |> ignore) id
        Expect.isTrue (e :? MidirException) (sprintf "MidirException expected, got %O" e)
        skiptestf "no ALSA sequencer: %s" e.Message
      for port in p.InputPorts do Expect.isNotEmpty port.Id "input id"
      for port in p.OutputPorts do Expect.isNotEmpty port.Id "output id"
      let ip = p :> IMidiPlatform<int64>
      Expect.equal ip.InputDevices.Length p.InputPorts.Length "IMidiPlatform inputs"
    }
    test "opening a missing port fails cleanly" {
      let p = platform.Force()
      let ghost = { Id = "midinette-does-not-exist"; Name = "ghost"; Direction = Input; IsVirtual = false }
      let input = p.GetInput ghost
      Expect.throwsT<MidirException> (fun () -> input.Open 0) "MidirException"
      Expect.isFalse input.IsOpen "still closed"
      input.Close() // no-op
      let out = p.GetOutput { ghost with Direction = Output }
      Expect.throwsT<MidirException> (fun () -> out.Open()) "MidirException"
    }
    test "virtual ports unsupported throws PlatformNotSupported" {
      let p = platform.Force()
      if p.SupportsVirtualPorts then skiptest "virtual ports supported here"
      Expect.throwsT<PlatformNotSupportedException> (fun () -> p.CreateVirtualOutput "x" |> ignore) "unsupported"
    }
  ]

[<Tests>]
let loopback =
  testSequenced <| testList "midir virtual loopback" [
    test "virtual output -> input: note, realtime, sysex" {
      requireVirtual ()
      let p = platform.Force()
      let name = uniqueName "Midinette Loop Out"
      use vout = p.CreateVirtualOutput name
      let port = findInput name |> Option.defaultWith (fun () -> failtest "virtual source not visible")
      use input = p.GetInput port
      use c = new Collector(input)
      input.Open 0
      vout.WriteMessage(MidiMessage.NoteOn (UMX.tag_byte_7bits 2uy) (tb 64uy) (tb 99uy))
      vout.WriteMessage(MidiMessage.Encode 0xf8uy 0uy 0uy)
      vout.WriteSysex [| 0xf0uy; 0x7euy; 0x7fuy; 0x06uy; 0x01uy; 0xf7uy |]
      let n = waitCount 3. (fun () -> c.Total) 3
      Expect.equal n 3 "3 messages"
      let ok, m = c.Channel.TryPeek()
      Expect.isTrue ok "note"
      Expect.equal (m.MessageType, m.Data1, m.Data2) (MidiMessageType.NoteOn, 64uy, 99uy) "note on"
      let _, rt = c.Realtime.TryPeek()
      Expect.equal rt.MessageType MidiMessageType.TimingClock "clock"
      let _, s = c.Sysex.TryPeek()
      Expect.equal s [| 0xf0uy; 0x7euy; 0x7fuy; 0x06uy; 0x01uy; 0xf7uy |] "identity request"
      Expect.isEmpty c.Errors "no errors"
    }
    test "output -> virtual input" {
      requireVirtual ()
      let p = platform.Force()
      let name = uniqueName "Midinette Loop In"
      use vin = p.CreateVirtualInput name
      use c = new Collector(vin)
      let port = findOutput name |> Option.defaultWith (fun () -> failtest "virtual destination not visible")
      use out = p.GetOutput port
      out.Open()
      for i in 0uy .. 9uy do
        out.WriteMessage(MidiMessage.CC (tb 0uy) (tb 7uy) (tb i))
      let n = waitCount 3. (fun () -> c.Channel.Count) 10
      Expect.equal n 10 "10 CCs"
      Expect.equal (c.Channel |> Seq.map (fun m -> m.Data2) |> List.ofSeq) [ 0uy .. 9uy ] "in order"
    }
    test "large sysex round-trip (>= 64 KB synthetic + machinedrum fixtures)" {
      requireVirtual ()
      let p = platform.Force()
      let name = uniqueName "Midinette Sysex"
      use vin = p.CreateVirtualInput name
      use c = new Collector(vin)
      let port = findOutput name |> Option.defaultWith (fun () -> failtest "virtual destination not visible")
      use out = p.GetOutput port
      out.Open()
      let payloads = bigSysex (70 * 1024) :: bigSysex (256 * 1024) :: fixtureSysex ()
      for s in payloads do out.WriteSysex s
      let n = waitCount 20. (fun () -> c.Sysex.Count) payloads.Length
      Expect.equal n payloads.Length "all sysex received"
      let got = List.ofSeq c.Sysex
      List.iteri2 (fun i (a: byte array) (b: byte array) ->
        Expect.equal a.Length b.Length (sprintf "length of sysex #%i" i)
        Expect.isTrue (a = b) (sprintf "content of sysex #%i" i)) got payloads
      Expect.isEmpty c.Errors "no errors"
    }
    test "open/close soak + idempotent dispose + no events after close" {
      requireVirtual ()
      let p = platform.Force()
      let name = uniqueName "Midinette Soak"
      use vout = p.CreateVirtualOutput name
      let port = findInput name |> Option.defaultWith (fun () -> failtest "virtual source not visible")
      let iterations = 1000
      let sw = Stopwatch.StartNew()
      for _ in 1 .. iterations do
        let input = p.GetInput port
        input.Open 0
        input.Close()
        input.Close()
        (input :> IDisposable).Dispose()
      let inputSoak = sw.Elapsed
      let vinName = uniqueName "Midinette Soak In"
      use _vin = p.CreateVirtualInput vinName
      let outPort = findOutput vinName |> Option.defaultWith (fun () -> failtest "virtual destination not visible")
      for _ in 1 .. iterations do
        let o = p.GetOutput outPort
        o.Open()
        o.Close()
        o.Close()
      printfn "soak: %i input open/close in %O, total %O" iterations inputSoak sw.Elapsed
      // no events after close
      let input = p.GetInput port
      let mutable afterClose = 0
      let mutable closed = false
      use _s = (input :> IMidiInput<int64>).ChannelMessageReceived.Subscribe(fun _ -> if closed then Interlocked.Increment(&afterClose) |> ignore)
      input.Open 0
      let sender = Thread(fun () ->
        for _ in 1 .. 2000 do
          try vout.WriteMessage(MidiMessage.NoteOn (UMX.tag_byte_7bits 0uy) (tb 1uy) (tb 1uy)) with _ -> ())
      sender.Start()
      Thread.Sleep 5
      input.Close()
      closed <- true
      sender.Join()
      Thread.Sleep 50
      Expect.equal afterClose 0 "no events after Close returned"
      // writing to a closed output fails cleanly
      let o = p.GetOutput outPort
      Expect.throwsT<InvalidOperationException> (fun () -> o.WriteMessage(MidiMessage.Encode 0xf8uy 0uy 0uy)) "closed output"
    }
    test "vanished virtual port fails cleanly on open" {
      requireVirtual ()
      let p = platform.Force()
      let name = uniqueName "Midinette Vanish"
      let vout = p.CreateVirtualOutput name
      let port = findInput name |> Option.defaultWith (fun () -> failtest "virtual source not visible")
      vout.Close()
      let gone = waitFor (TimeSpan.FromSeconds 3.) (fun () -> if (p.TryFindInput name).IsNone then Some () else None)
      Expect.isSome gone "port disappeared from list (hot-unplug)"
      let input = p.GetInput port
      Expect.throwsT<MidirException> (fun () -> input.Open 0) "MidirException on vanished port"
    }
    test "IVirtualMidiPlatform via interface" {
      requireVirtual ()
      let vp = platform.Force() :> IVirtualMidiPlatform<int64>
      Expect.isTrue vp.SupportsVirtualPorts "supported"
      let o = vp.CreateVirtualOutput(uniqueName "Midinette IFace")
      o.Close()
      o.Close()
    }
  ]
