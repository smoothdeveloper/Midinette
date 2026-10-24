/// Lists MIDI ports, creates "Midinette In" / "Midinette Out" virtual ports (where the OS
/// allows it) and echoes everything received on "Midinette In" to the console and to
/// "Midinette Out".
///   dotnet run --project samples/MidirMonitor              (until Ctrl+C)
///   dotnet run --project samples/MidirMonitor -- --seconds 5
module MidirMonitor.Program
open System
open System.Threading
open Midi
open Midinette.Platform
open Midinette.Platform.Midir

[<EntryPoint>]
let main args =
  let seconds =
    match Array.tryFindIndex ((=) "--seconds") args with
    | Some i when i + 1 < args.Length -> Some (int args.[i + 1])
    | _ -> None
  let platform = MidirPlatform("Midinette")
  printfn "midir backend: %s, virtual ports: %b" platform.BackendName platform.SupportsVirtualPorts
  printfn "inputs:"
  for p in platform.InputPorts do printfn "  %-40s %s" p.Name p.Id
  printfn "outputs:"
  for p in platform.OutputPorts do printfn "  %-40s %s" p.Name p.Id

  if not platform.SupportsVirtualPorts then
    printfn "virtual ports are not supported here (Windows: use loopMIDI or Windows MIDI Services)."
    0
  else
    use vout = platform.CreateVirtualOutput "Midinette Out"
    use vin = platform.CreateVirtualInput "Midinette In"
    let input = vin :> IMidiInput<int64>
    use _c = input.ChannelMessageReceived.Subscribe(fun e ->
      printfn "%10d  %O" e.Timestamp e.Message
      vout.WriteMessage e.Message)
    use _r = input.RealtimeMessageReceived.Subscribe(fun e ->
      let message = e.Message
      let kind = message.MessageType
      if kind <> MidiMessageType.TimingClock && kind <> MidiMessageType.ActiveSensing then
        printfn "%10d  %O" e.Timestamp e.Message
      vout.WriteMessage e.Message)
    use _s = input.SysexReceived.Subscribe(fun s ->
      let bytes = UMX.untag_sysex s
      let preview = bytes |> Array.truncate 16 |> Array.map (sprintf "%02X") |> String.concat " "
      printfn "            sysex %d bytes: %s%s" bytes.Length preview (if bytes.Length > 16 then " ..." else "")
      vout.WriteSysex bytes)
    use _e = input.Error.Subscribe(fun e -> eprintfn "error: %s" e)
    printfn "listening on virtual 'Midinette In', echoing to virtual 'Midinette Out'%s"
      (match seconds with Some s -> sprintf " for %ds" s | None -> " (Ctrl+C to quit)")
    use stop = new ManualResetEventSlim(false)
    Console.CancelKeyPress.Add(fun e -> e.Cancel <- true; stop.Set())
    match seconds with
    | Some s -> stop.Wait(TimeSpan.FromSeconds(float s)) |> ignore
    | None -> stop.Wait()
    0
