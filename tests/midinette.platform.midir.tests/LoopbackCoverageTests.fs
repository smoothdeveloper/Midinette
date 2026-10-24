/// Exhaustive / fuzz / stress coverage of midir through real OS ports: a virtual port is
/// looped back into the same process and everything received is compared, in order and
/// byte for byte, with what was sent. Skipped where midir has no virtual ports.
module midinette.platform.midir.tests.LoopbackCoverageTests
open System
open System.Collections.Concurrent
open System.Diagnostics
open Expecto
open midi
open midi.ByteStream
open midinette.platform
open midinette.platform.midir
open midinette.platform.midir.tests.MidirTests

/// everything an input raises, in arrival order (events come from one dispatcher thread)
type OrderedCapture(input: IMidiInput<int64>) =
  let items = ConcurrentQueue<byte array>()
  let stamps = ConcurrentQueue<int64>()
  let errors = ConcurrentQueue<string>()
  let onMessage (e: MidiEvent<int64>) =
    stamps.Enqueue e.Timestamp
    items.Enqueue(toBytes e.Message)
  let subs = [
    input.ChannelMessageReceived.Subscribe onMessage
    input.SystemMessageReceived.Subscribe onMessage
    input.RealtimeMessageReceived.Subscribe onMessage
    input.SysexReceived.Subscribe(fun s -> items.Enqueue(UMX.untag_sysex s))
    input.Error.Subscribe errors.Enqueue ]
  member _.Count = items.Count
  member _.Items = List.ofSeq items
  member _.Timestamps = Array.ofSeq stamps
  member _.Errors = List.ofSeq errors
  interface IDisposable with
    member _.Dispose() = for s in subs do s.Dispose()

type Direction =
  /// virtual output (source) -> regular input connection
  | VirtualOutToInput
  /// regular output connection -> virtual input (destination)
  | OutputToVirtualIn

/// runs `f send capture` over a fresh loopback; `send` takes raw wire bytes (one packet/call)
let withLoop direction (f: (byte array -> unit) -> OrderedCapture -> unit) =
  requireVirtual ()
  let p = platform.Force()
  match direction with
  | VirtualOutToInput ->
    let name = uniqueName "Midinette Cover Out"
    use vout = p.CreateVirtualOutput name
    let port = findInput name |> Option.defaultWith (fun () -> failtest "virtual source not visible")
    use input = p.GetInput port
    use c = new OrderedCapture(input)
    input.Open 0
    f vout.Send c
  | OutputToVirtualIn ->
    let name = uniqueName "Midinette Cover In"
    use vin = p.CreateVirtualInput name
    use c = new OrderedCapture(vin)
    let port = findOutput name |> Option.defaultWith (fun () -> failtest "virtual destination not visible")
    use out = p.GetOutput port
    out.Open()
    f out.Send c

let hex (b: byte array) =
  if b.Length > 12 then sprintf "%s .. %s (%i bytes)" (BitConverter.ToString b.[.. 5]) (BitConverter.ToString b.[b.Length - 3 ..]) b.Length
  else BitConverter.ToString b

let expectSame (label: string) (expected: byte array list) (c: OrderedCapture) (timeoutSeconds: float) =
  let n = waitCount timeoutSeconds (fun () -> c.Count) expected.Length
  // grace period: anything extra (duplicates) must show up too
  Threading.Thread.Sleep 50
  let got = c.Items
  if n <> expected.Length || got.Length <> expected.Length || not (List.forall2 (fun (a: byte array) b -> a = b) got expected) then
    let firstDiff =
      Seq.zip got expected |> Seq.tryFindIndex (fun (a, b) -> a <> b)
      |> Option.map (fun i -> sprintf "first difference at #%i: got %s expected %s" i (hex got.[i]) (hex expected.[i]))
      |> Option.defaultValue "common prefix identical"
    if got.Length < 20 then printfn "%s got: %A" label (got |> List.map hex)
    failtestf "%s: received %i/%i messages; %s; errors %A" label got.Length expected.Length firstDiff (List.truncate 5 c.Errors)
  Expect.isEmpty c.Errors (label + ": no parser errors")

// ------------------------------------------------------------------ message catalog

let channelVoice =
  [ for ch in 0uy .. 15uy do
      for (d1, d2) in [ 0uy, 0uy; 60uy, 100uy; 127uy, 127uy ] do
        yield [| 0x80uy + ch; d1; d2 |]   // note off
        yield [| 0x90uy + ch; d1; d2 |]   // note on (incl. velocity 0)
        yield [| 0xa0uy + ch; d1; d2 |]   // poly pressure
        yield [| 0xb0uy + ch; d1; d2 |]   // control change
        yield [| 0xe0uy + ch; d1; d2 |]   // pitch bend (min, ~centre, max)
      yield [| 0xb0uy + ch; 123uy; 0uy |] // all notes off
      yield [| 0xe0uy + ch; 0uy; 0x40uy |] // pitch bend centre
      for v in [ 0uy; 64uy; 127uy ] do
        yield [| 0xc0uy + ch; v |]        // program change
        yield [| 0xd0uy + ch; v |] ]      // channel pressure

let systemCommon =
  [ for qf in [ 0x00uy; 0x17uy; 0x7fuy ] do yield [| 0xf1uy; qf |]  // MTC quarter frame
    yield [| 0xf2uy; 0uy; 0uy |]; yield [| 0xf2uy; 0x7fuy; 0x7fuy |]; yield [| 0xf2uy; 0x10uy; 0x02uy |] // song position
    yield [| 0xf3uy; 0uy |]; yield [| 0xf3uy; 127uy |]                  // song select
    yield [| 0xf6uy |] ]                                                 // tune request

let realtime =
  [ 0xf8uy; 0xfauy; 0xfbuy; 0xfcuy; 0xfeuy; 0xffuy ] |> List.map (fun b -> [| b |]) // clock start continue stop sense reset

let sysexSizes = [ 2; 3; 4; 5; 16; 127; 128; 255; 256; 257; 1023; 1024; 4095; 4096; 16383; 16384; 16385; 65535; 65536; 65537 ]
let sysexOf size = bigSysex size

let catalog = channelVoice @ systemCommon @ realtime

// ------------------------------------------------------------------ fuzz generator

let randomMessage (rnd: Random) =
  let d () = byte (rnd.Next 128)
  match rnd.Next 100 with
  | n when n < 70 ->
    let status = byte (0x80 + rnd.Next(0x70))
    if dataLength status = 1 then [| status; d () |] else [| status; d (); d () |]
  | n when n < 78 ->
    match rnd.Next 4 with
    | 0 -> [| 0xf1uy; d () |]
    | 1 -> [| 0xf2uy; d (); d () |]
    | 2 -> [| 0xf3uy; d () |]
    | _ -> [| 0xf6uy |]
  | n when n < 90 -> [| [| 0xf8uy; 0xfauy; 0xfbuy; 0xfcuy; 0xfeuy; 0xffuy |].[rnd.Next 6] |]
  | _ ->
    let len = if rnd.Next 10 = 0 then rnd.Next(2, 20000) else rnd.Next(2, 300)
    Array.init len (fun i -> if i = 0 then 0xf0uy elif i = len - 1 then 0xf7uy else d ())

/// groups consecutive messages into packets of 1..maxGroup messages
let randomPackets (rnd: Random) maxGroup (messages: byte array list) =
  let rec go acc (rest: byte array list) =
    match rest with
    | [] -> List.rev acc
    | _ ->
      let n = min rest.Length (rnd.Next(1, maxGroup + 1))
      go (Array.concat (List.take n rest) :: acc) (List.skip n rest)
  go [] messages

let directions = [ VirtualOutToInput; OutputToVirtualIn ]

[<Tests>]
let coverage =
  testSequenced <| testList "midir loopback coverage" [
    for dir in directions do
      test (sprintf "%A: every channel voice / system common / realtime message" dir) {
        withLoop dir (fun send c ->
          for m in catalog do send m
          expectSame "catalog" catalog c 10.)
      }
      test (sprintf "%A: sysex sizes 2 B .. 64 KB+1 (incl. empty F0 F7)" dir) {
        withLoop dir (fun send c ->
          let all = sysexSizes |> List.map sysexOf
          for s in all do send s
          expectSame "sysex sizes" all c 20.)
      }
      test (sprintf "%A: several messages + sysex in one send keep order" dir) {
        withLoop dir (fun send c ->
          let msgs = [ [| 0x90uy; 60uy; 100uy |]; sysexOf 10; [| 0x80uy; 60uy; 0uy |]; [| 0xf8uy |]; sysexOf 3000; [| 0xc5uy; 9uy |] ]
          send (Array.concat msgs)
          expectSame "mixed packet" msgs c 5.)
      }
      test (sprintf "%A: running status in a raw send arrives as full messages" dir) {
        withLoop dir (fun send c ->
          send [| 0x90uy; 60uy; 100uy; 61uy; 101uy; 0xf8uy; 62uy; 102uy; 0xc3uy; 1uy; 2uy; 0xe0uy; 0uy; 0x40uy; 0x7fuy; 0x7fuy |]
          let expected =
            [ [| 0x90uy; 60uy; 100uy |]; [| 0x90uy; 61uy; 101uy |]; [| 0xf8uy |]; [| 0x90uy; 62uy; 102uy |]
              [| 0xc3uy; 1uy |]; [| 0xc3uy; 2uy |]; [| 0xe0uy; 0uy; 0x40uy |]; [| 0xe0uy; 0x7fuy; 0x7fuy |] ]
          expectSame "running status" expected c 5.)
      }
      test (sprintf "%A: realtime interleaved inside sysex" dir) {
        withLoop dir (fun send c ->
          send [| 0xf0uy; 0x7duy; 1uy; 0xf8uy; 2uy; 0xfeuy; 3uy; 0xf7uy |]
          expectSame "realtime in sysex" [ [| 0xf8uy |]; [| 0xfeuy |]; [| 0xf0uy; 0x7duy; 1uy; 2uy; 3uy; 0xf7uy |] ] c 5.)
      }
      test (sprintf "%A: fuzz, random well-formed streams (seeded)" dir) {
        withLoop dir (fun send c ->
          let mutable expected = []
          for seed in 1 .. 4 do
            let rnd = Random(seed * 7919)
            let msgs = List.init 1500 (fun _ -> randomMessage rnd)
            for packet in randomPackets rnd 8 msgs do send packet
            expected <- expected @ msgs
          expectSame "fuzz" expected c 30.)
      }
      test (sprintf "%A: stress flood, no loss, order and monotonic timestamps" dir) {
        withLoop dir (fun send c ->
          let n = 20000
          let msgs = List.init n (fun i -> [| 0x90uy + byte (i % 16); byte (i % 128); byte ((i / 128) % 128) |])
          let sw = Stopwatch.StartNew()
          for m in msgs do send m
          let sent = sw.Elapsed
          let batched = List.init 50000 (fun i -> [| 0xb0uy + byte (i % 16); byte ((i / 16) % 128); byte (i % 128) |])
          for packet in List.chunkBySize 64 batched do send (Array.concat packet)
          expectSame "flood" (msgs @ batched) c 60.
          printfn "flood %A: %i single sends in %O, +%i batched, all received in %O" dir n sent batched.Length sw.Elapsed
          let ts = c.Timestamps
          Expect.isTrue (ts |> Array.pairwise |> Array.forall (fun (a, b) -> b >= a)) "timestamps never go back")
      }
      test (sprintf "%A: bad wire bytes do not crash, stream recovers" dir) {
        withLoop dir (fun send c ->
          let rnd = Random 1337
          for _ in 1 .. 200 do
            send (Array.init (rnd.Next(1, 64)) (fun _ -> byte (rnd.Next 256)))
          // close any sysex the garbage left open, then a sentinel
          send [| 0xf7uy |]
          send [| 0x9fuy; 0x55uy; 0x66uy |]
          let found = waitFor (TimeSpan.FromSeconds 5.) (fun () -> if c.Items |> List.exists (fun b -> b = [| 0x9fuy; 0x55uy; 0x66uy |]) then Some () else None)
          Expect.isSome found "sentinel received after garbage"
          for b in c.Items do
            match b with
            | [||] -> failtest "empty message"
            | x when x.[0] = 0xf0uy -> Expect.equal x.[x.Length - 1] 0xf7uy "sysex terminated"
            | x -> Expect.equal x.Length (messageLength (MidiMessage.Encode x.[0] 0uy 0uy)) (sprintf "wire length of %s" (hex x)))
      }
    test "close while flooding: no crash, no event after close (200 cycles)" {
      // regression: midir's CoreMIDI close() raced the read callback and aborted the process
      requireVirtual ()
      let p = platform.Force()
      let name = uniqueName "Midinette Close Race"
      use vout = p.CreateVirtualOutput name
      let port = findInput name |> Option.defaultWith (fun () -> failtest "virtual source not visible")
      use stop = new Threading.CancellationTokenSource()
      let sender = Threading.Thread(fun () ->
        while not stop.IsCancellationRequested do
          try vout.Send [| 0x90uy; 1uy; 1uy; 0xf8uy; 0xf0uy; 0x7duy; 1uy; 0xf7uy |] with _ -> ())
      sender.Start()
      let mutable late = 0
      for _ in 1 .. 200 do
        let input = p.GetInput port
        let mutable closed = false
        use _s = (input :> IMidiInput<int64>).RealtimeMessageReceived.Subscribe(fun _ -> if closed then Threading.Interlocked.Increment(&late) |> ignore)
        input.Open 0
        Threading.Thread.Sleep 1
        input.Close()
        closed <- true
      stop.Cancel()
      sender.Join()
      Expect.equal late 0 "no events after Close returned"
    }
  ]

// ------------------------------------------------------------------ pure parser abuse

let feedAll (p: MidiByteStreamParser) (chunks: byte array seq) =
  let out = ResizeArray()
  for c in chunks do p.Feed(c, out.Add)
  List.ofSeq out

let checkWellFormed (maxSysex: int) (r: ParsedMidi list) =
  for x in r do
    match x with
    | ParsedMidi.Channel m ->
      Expect.isTrue (m.Status >= 0x80uy && m.Status < 0xf0uy) "channel status"
      Expect.isTrue (m.Data1 < 0x80uy && m.Data2 < 0x80uy) "channel data 7 bits"
    | ParsedMidi.SystemCommon m -> Expect.isTrue (m.Status >= 0xf1uy && m.Status <= 0xf7uy) "system common status"
    | ParsedMidi.Realtime m -> Expect.isTrue (m.Status >= 0xf8uy && m.Status <> 0xf9uy && m.Status <> 0xfduy) "realtime status"
    | ParsedMidi.Sysex s ->
      Expect.equal s.[0] 0xf0uy "F0"
      Expect.equal s.[s.Length - 1] 0xf7uy "F7"
      Expect.isTrue (s.[1 .. s.Length - 2] |> Array.forall (fun b -> b < 0x80uy)) "sysex body is data"
      Expect.isLessThanOrEqual (s.Length - 1) maxSysex "within MaxSysexLength"
    | ParsedMidi.Dropped _ -> ()

[<Tests>]
let parserAbuse =
  testList "bytestream abuse" [
    test "random garbage never throws and only emits well-formed messages" {
      for seed in 1 .. 20 do
        let rnd = Random seed
        let p = MidiByteStreamParser(4096)
        let chunks = [ for _ in 1 .. 500 -> Array.init (rnd.Next(0, 200)) (fun _ -> byte (rnd.Next 256)) ]
        feedAll p chunks |> checkWellFormed 4096
    }
    test "random well-formed stream survives any chunking" {
      let rnd = Random 4242
      let msgs = List.init 5000 (fun _ -> randomMessage rnd)
      let all = Array.concat msgs
      for chunkMax in [ 1; 2; 3; 7; 64; 4096 ] do
        let chunks = ResizeArray<byte array>()
        let mutable i = 0
        while i < all.Length do
          let n = min (all.Length - i) (rnd.Next(1, chunkMax + 1))
          chunks.Add all.[i .. i + n - 1]
          i <- i + n
        let got =
          feedAll (MidiByteStreamParser()) chunks
          |> List.map (function
            | ParsedMidi.Channel m | ParsedMidi.SystemCommon m | ParsedMidi.Realtime m -> toBytes m
            | ParsedMidi.Sysex s -> s
            | ParsedMidi.Dropped (r, _) -> failtestf "unexpected drop: %s" r)
        Expect.equal got.Length msgs.Length (sprintf "count, chunks <= %i" chunkMax)
        Expect.isTrue (List.forall2 (fun (a: byte array) b -> a = b) got msgs) (sprintf "bytes, chunks <= %i" chunkMax)
    }
    test "edge streams" {
      let cases =
        [ "truncated sysex then note", [| 0xf0uy; 1uy; 2uy; 0x90uy; 60uy; 100uy |]
          "orphan F7", [| 0xf7uy; 0x90uy; 1uy; 2uy |]
          "F0 F0 F7", [| 0xf0uy; 0xf0uy; 0xf7uy |]
          "status without data then status", [| 0x90uy; 0x80uy; 1uy; 2uy |]
          "undefined F4 F5 F9 FD", [| 0xf4uy; 0xf5uy; 0xf9uy; 0xfduy; 0xf8uy |]
          "system common breaks running status", [| 0x90uy; 1uy; 2uy; 0xf6uy; 3uy; 4uy |]
          "all data", Array.init 300 (fun i -> byte (i % 128))
          "all status", Array.init 128 (fun i -> byte (0x80 + i)) ]
      for (name, bytes) in cases do
        let r = feedAll (MidiByteStreamParser(16)) [ bytes ]
        checkWellFormed 16 r
        // same stream byte by byte must give the same result
        let r1 = feedAll (MidiByteStreamParser(16)) [ for b in bytes -> [| b |] ]
        Expect.equal (sprintf "%A" r1) (sprintf "%A" r) (name + ": chunking-independent")
    }
    test "overlong sysex is dropped and parser recovers" {
      let p = MidiByteStreamParser(8)
      let r = feedAll p [ bigSysex 100; [| 0x90uy; 60uy; 100uy |]; bigSysex 8 ]
      match r with
      | ParsedMidi.Dropped _ :: rest ->
        let ok = rest |> List.exists (function ParsedMidi.Channel m -> m.Data1 = 60uy | _ -> false)
        Expect.isTrue ok "note after overlong sysex"
        Expect.isTrue (rest |> List.exists (function ParsedMidi.Sysex s -> s.Length = 8 | _ -> false)) "sysex at the limit delivered"
      | other -> failtestf "unexpected %A" other
    }
  ]
