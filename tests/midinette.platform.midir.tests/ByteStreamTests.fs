/// Pure parser tests: no MIDI hardware or OS ports needed.
module midinette.platform.midir.tests.ByteStreamTests
open System
open System.IO
open Expecto
open midi
open midi.ByteStream

let parse (chunks: byte array list) =
  let p = MidiByteStreamParser()
  let out = ResizeArray()
  for c in chunks do p.Feed(c, out.Add)
  List.ofSeq out

let fixturesDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "data", "machinedrum")
let fixtureMessages () =
  if Directory.Exists fixturesDir then
    [ for f in Directory.GetFiles(fixturesDir, "*.syx") do
        yield! midinette.sysex.Sysex.getSysexMessagesFromBytes (File.ReadAllBytes f) |> Array.map UMX.untag_sysex ]
  else []

[<Tests>]
let tests =
  testList "bytestream" [
    test "note on" {
      match parse [ [| 0x91uy; 60uy; 100uy |] ] with
      | [ ParsedMidi.Channel m ] ->
        Expect.equal m.MessageType MidiMessageType.NoteOn "type"
        Expect.equal m.Channel (Some (UMX.tag_byte_7bits 1uy)) "channel"
        Expect.equal (m.Data1, m.Data2) (60uy, 100uy) "data"
      | other -> failtestf "unexpected %A" other
    }
    test "running status, split over chunks" {
      let r = parse [ [| 0x90uy; 60uy |]; [| 100uy; 61uy; 101uy; 62uy |]; [| 0uy |] ]
      Expect.equal r.Length 3 "three notes"
      for x in r do
        match x with
        | ParsedMidi.Channel m -> Expect.equal m.MessageType MidiMessageType.NoteOn "note on"
        | other -> failtestf "unexpected %A" other
    }
    test "program change has one data byte" {
      match parse [ [| 0xc2uy; 5uy; 6uy |] ] with
      | [ ParsedMidi.Channel a; ParsedMidi.Channel b ] ->
        Expect.equal (a.Data1, b.Data1) (5uy, 6uy) "running status program changes"
      | other -> failtestf "unexpected %A" other
    }
    test "realtime inside sysex is split out" {
      let r = parse [ [| 0xf0uy; 0x7duy; 0xf8uy; 1uy; 2uy |]; [| 0xfeuy; 3uy; 0xf7uy |] ]
      match r with
      | [ ParsedMidi.Realtime clock; ParsedMidi.Realtime sense; ParsedMidi.Sysex s ] ->
        Expect.equal clock.MessageType MidiMessageType.TimingClock "clock"
        Expect.equal sense.MessageType MidiMessageType.ActiveSensing "active sensing"
        Expect.equal s [| 0xf0uy; 0x7duy; 1uy; 2uy; 3uy; 0xf7uy |] "sysex reassembled"
      | other -> failtestf "unexpected %A" other
    }
    test "system common" {
      match parse [ [| 0xf2uy; 1uy; 2uy; 0xf6uy; 0xf3uy; 7uy |] ] with
      | [ ParsedMidi.SystemCommon spp; ParsedMidi.SystemCommon tune; ParsedMidi.SystemCommon song ] ->
        Expect.equal spp.MessageType MidiMessageType.SongPositionPointer "spp"
        Expect.equal tune.MessageType MidiMessageType.TuneRequest "tune"
        Expect.equal (song.MessageType, song.Data1) (MidiMessageType.SongSelect, 7uy) "song select"
      | other -> failtestf "unexpected %A" other
    }
    test "sysex aborted by status byte is reported, not delivered" {
      match parse [ [| 0xf0uy; 1uy; 2uy; 0x90uy; 60uy; 1uy |] ] with
      | [ ParsedMidi.Dropped _; ParsedMidi.Channel _ ] -> ()
      | other -> failtestf "unexpected %A" other
    }
    test "orphan data is dropped" {
      match parse [ [| 1uy; 2uy |] ] with
      | [ ParsedMidi.Dropped _; ParsedMidi.Dropped _ ] -> ()
      | other -> failtestf "unexpected %A" other
    }
    test "toBytes uses wire length" {
      Expect.equal (toBytes (MidiMessage.Encode 0xc0uy 3uy 0uy)) [| 0xc0uy; 3uy |] "program change"
      Expect.equal (toBytes (MidiMessage.Encode 0xf8uy 0uy 0uy)) [| 0xf8uy |] "clock"
      Expect.equal (toBytes (MidiMessage.Encode 0x80uy 1uy 2uy)) [| 0x80uy; 1uy; 2uy |] "note off"
    }
    test "machinedrum fixtures survive random chunking" {
      let messages = fixtureMessages ()
      if messages.IsEmpty then skiptest "no data/machinedrum fixtures"
      let all = Array.concat messages
      let rnd = Random(42)
      let chunks =
        [ let mutable i = 0
          while i < all.Length do
            let n = min (all.Length - i) (rnd.Next(1, 300))
            yield all.[i .. i + n - 1]
            i <- i + n ]
      let r = parse chunks
      let got = r |> List.choose (function ParsedMidi.Sysex s -> Some s | _ -> None)
      Expect.equal got.Length messages.Length "same message count"
      Expect.isTrue (List.forall2 (fun (a: byte array) b -> a = b) got messages) "byte-identical"
    }
  ]
