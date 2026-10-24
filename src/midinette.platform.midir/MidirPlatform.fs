namespace midinette.platform.midir
#nowarn "9"
#nowarn "51"
open System
open System.Collections.Concurrent
open System.Runtime.InteropServices
open System.Threading
open Midi
open Midi.ByteStream
open midinette.platform

type MidirException(message: string, code: int) =
  inherit Exception(message)
  member _.Code = code

type PortDirection =
  | Input
  | Output

/// A port as seen by midir. `Id` is midir's stable port id (CoreMIDI unique id, ALSA client:port, ...).
[<CustomEquality; NoComparison>]
type MidirPortInfo =
  { Id: string; Name: string; Direction: PortDirection; IsVirtual: bool }
  interface IDeviceInfo with
    member x.Name = x.Name
  override x.ToString() = sprintf "%A %s (%s)" x.Direction x.Name x.Id
  override x.Equals o =
    match o with
    | :? MidirPortInfo as y -> x.Id = y.Id && x.Direction = y.Direction
    | _ -> false
  override x.GetHashCode() = hash (x.Id, x.Direction)

/// receives raw bytes from the native callback (midir thread)
type internal IRawSink =
  abstract Push: timestampUs: int64 * bytes: byte array -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type internal InputCallback = delegate of nativeint * uint64 * nativeint * unativeint -> unit

/// one process-wide native callback (F# has no UnmanagedCallersOnly support): a blittable
/// delegate rooted in a static field forever; per-connection routing via a GCHandle user_data.
type internal Callbacks private () =
  static let onInput (userData: nativeint) (timestampUs: uint64) (data: nativeint) (len: unativeint) : unit =
    try
      let h = GCHandle.FromIntPtr userData
      match h.Target with
      | :? IRawSink as sink ->
        let bytes = Array.zeroCreate<byte> (int len)
        if bytes.Length > 0 then Marshal.Copy(data, bytes, 0, bytes.Length)
        sink.Push(int64 timestampUs, bytes)
      | _ -> ()
    with _ -> ()
  // must stay referenced by a member: an unused `static let` is compiled to a local and the
  // delegate would be collected while midir still holds its thunk.
  static let callback = InputCallback(fun ud ts d l -> onInput ud ts d l)
  static let pointer = Marshal.GetFunctionPointerForDelegate callback
  static member Delegate : InputCallback = callback
  static member Pointer : nativeint = GC.KeepAlive callback; pointer

module internal Check =
  let ensure code what =
    if code <> Native.Ok then
      raise (MidirException(sprintf "%s: %s" what (Native.lastError ()), code))

/// `IMidiInput` over a midir input connection. Timestamps are midir's microseconds.
/// Events are raised on a dedicated dispatcher thread (never on midir's thread); after
/// `Close`/`Dispose` returns, no further events are raised.
type MidirInput internal (info: MidirPortInfo, platformEvents: MidiPlatformTrigger<int64>, opener: nativeint -> nativeint -> nativeint) =
  let error           = Event<string>()
  let channelMessage  = Event<MidiEvent<int64>>()
  let systemMessage   = Event<MidiEvent<int64>>()
  let sysex           = Event<sysex_data>()
  let realtimeMessage = Event<MidiEvent<int64>>()
  let gate = obj ()
  let parser = MidiByteStreamParser()
  let mutable handle = 0n
  let mutable gcHandle = Unchecked.defaultof<GCHandle>
  let mutable queue : BlockingCollection<struct (int64 * byte array)> = null
  let mutable dispatcher : Thread = null
  let mutable closed = true
  let deviceInfo = info :> IDeviceInfo

  let raiseParsed (ts: int64) (p: ParsedMidi) =
    match p with
    | ParsedMidi.Channel m ->
      let e = MidiEvent(m, ts)
      channelMessage.Trigger e
      platformEvents.NoticeChannelMessage(deviceInfo, e)
    | ParsedMidi.SystemCommon m ->
      let e = MidiEvent(m, ts)
      systemMessage.Trigger e
      platformEvents.NoticeSystemMessage(deviceInfo, e)
    | ParsedMidi.Realtime m ->
      let e = MidiEvent(m, ts)
      realtimeMessage.Trigger e
      platformEvents.NoticeRealtimeMessage(deviceInfo, e)
    | ParsedMidi.Sysex bytes ->
      sysex.Trigger(UMX.tag_sysex_data bytes)
      platformEvents.NoticeSysex(deviceInfo, bytes)
    | ParsedMidi.Dropped (reason, bytes) ->
      let msg = sprintf "%s (%i bytes)" reason bytes.Length
      error.Trigger msg
      platformEvents.NoticeError(deviceInfo, msg)

  let dispatch (q: BlockingCollection<struct (int64 * byte array)>) () =
    for struct (ts, bytes) in q.GetConsumingEnumerable() do
      lock gate (fun () ->
        if not closed then
          try parser.Feed(bytes, raiseParsed ts)
          with ex ->
            let msg = sprintf "event handler failed: %s" ex.Message
            try error.Trigger msg; platformEvents.NoticeError(deviceInfo, msg) with _ -> ())

  member _.Info = info
  member _.IsOpen = not closed

  member this.Open(_bufferSize: int) =
    lock gate (fun () ->
      if closed then
        let q = new BlockingCollection<_>()
        let gh = GCHandle.Alloc(this :> IRawSink)
        let h =
          try opener Callbacks.Pointer (GCHandle.ToIntPtr gh)
          with _ -> gh.Free(); q.Dispose(); reraise ()
        handle <- h
        gcHandle <- gh
        queue <- q
        parser.Reset()
        closed <- false
        let t = Thread(dispatch q, IsBackground = true, Name = sprintf "midinette-midir-in %s" info.Name)
        dispatcher <- t
        t.Start())

  member _.Close() =
    let toJoin =
      lock gate (fun () ->
        if closed then None
        else
          closed <- true
          // after this returns, midir guarantees the native callback won't run again
          Native.mmidir_input_close handle |> ignore
          handle <- 0n
          gcHandle.Free()
          queue.CompleteAdding()
          let t, q = dispatcher, queue
          dispatcher <- null
          queue <- null
          Some (t, q))
    match toJoin with
    | Some (t, q) ->
      if not (obj.ReferenceEquals(Thread.CurrentThread, t)) then
        t.Join()
        q.Dispose()
    | None -> ()

  interface IRawSink with
    member _.Push(ts, bytes) =
      let q = queue
      if not (isNull q) then
        try q.Add(struct (ts, bytes)) with :? InvalidOperationException | :? ObjectDisposedException -> ()

  interface IMidiInput<int64> with
    [<CLIEvent>] member _.Error = error.Publish
    [<CLIEvent>] member _.ChannelMessageReceived = channelMessage.Publish
    [<CLIEvent>] member _.SystemMessageReceived = systemMessage.Publish
    [<CLIEvent>] member _.SysexReceived = sysex.Publish
    [<CLIEvent>] member _.RealtimeMessageReceived = realtimeMessage.Publish
    member this.Open bufferSize = this.Open bufferSize
    member this.Close() = this.Close()
    member _.DeviceInfo = deviceInfo

  interface IDisposable with
    member this.Dispose() = this.Close()

/// `IMidiOutput` over a midir output connection. Messages are sent immediately
/// (the timestamp argument is ignored: midir has no scheduled output).
type MidirOutput internal (info: MidirPortInfo, opener: unit -> nativeint) =
  let gate = obj ()
  let mutable handle = 0n
  let deviceInfo = info :> IDeviceInfo

  let send (bytes: byte array) =
    lock gate (fun () ->
      if handle = 0n then raise (InvalidOperationException(sprintf "output %s is not open" info.Name))
      let code = Native.withBytes bytes (fun p l -> Native.mmidir_output_send(handle, p, l))
      Check.ensure code (sprintf "send to %s" info.Name))

  member _.Info = info
  member _.IsOpen = handle <> 0n
  member _.Open() =
    lock gate (fun () -> if handle = 0n then handle <- opener ())
  member _.Close() =
    lock gate (fun () ->
      if handle <> 0n then
        Native.mmidir_output_close handle |> ignore
        handle <- 0n)
  /// send raw bytes (one or more complete messages)
  member _.Send(bytes: byte array) = send bytes
  member _.WriteMessage(message: MidiMessage) = send (ByteStream.toBytes message)
  member _.WriteSysex(data: byte array) = send data

  interface IMidiOutput<int64> with
    member this.Open _bufferSize _latency = this.Open()
    member this.Close() = this.Close()
    member _.DeviceInfo = deviceInfo
    member _.WriteMessage _timestamp message = send (ByteStream.toBytes message)
    member _.WriteMessages _timestamp messages = send (messages |> Array.collect ByteStream.toBytes)
    member _.WriteSysex _timestamp data = send (UMX.untag_sysex data)

  interface IDisposable with
    member this.Dispose() = this.Close()

/// Midinette platform over the Rust crate `midir` (CoreMIDI / ALSA / WinMM).
/// Port lists are re-enumerated on every access (hot-plug friendly).
type MidirPlatform(clientName: string) =
  do
    let v = Native.mmidir_abi_version()
    if v <> Native.ExpectedAbiVersion then
      raise (MidirException(sprintf "midinette_midir native ABI %i, expected %i" v Native.ExpectedAbiVersion, -1))
  let trigger = MidiPlatformTrigger<int64>()

  let listPorts direction =
    let dir = match direction with Input -> 0 | Output -> 1
    match Native.readString (fun b c -> Native.mmidir_list_ports(dir, b, c)) with
    | Error code -> raise (MidirException(sprintf "list ports: %s" (Native.lastError ()), code))
    | Ok text ->
      text.Split('\n', StringSplitOptions.RemoveEmptyEntries)
      |> Array.map (fun line ->
        match line.Split('\t', 2) with
        | [| id; name |] -> { Id = id; Name = name; Direction = direction; IsVirtual = false }
        | _ -> { Id = line; Name = line; Direction = direction; IsVirtual = false })

  let openInput (port: MidirPortInfo) cb ud =
    let mutable h = 0n
    let code =
      Native.withUtf8 port.Id (fun id idLen ->
        Native.withUtf8 clientName (fun c cLen ->
          Native.mmidir_input_open(id, idLen, c, cLen, cb, ud, &&h)))
    Check.ensure code (sprintf "open input %s" port.Name)
    h

  let openOutput (port: MidirPortInfo) () =
    let mutable h = 0n
    let code =
      Native.withUtf8 port.Id (fun id idLen ->
        Native.withUtf8 clientName (fun c cLen ->
          Native.mmidir_output_open(id, idLen, c, cLen, &&h)))
    Check.ensure code (sprintf "open output %s" port.Name)
    h

  new() = MidirPlatform("midinette")

  member _.ClientName = clientName
  /// platform-wide events (all opened inputs)
  member _.Events = trigger :> IMidiPlatformEvents<int64>
  member _.Trigger = trigger
  /// midir backend compiled into the native library ("coremidi", "alsa", "winmm", ...)
  member _.BackendName =
    match Native.readString (fun b c -> Native.mmidir_backend_name(b, c)) with
    | Ok s -> s
    | Error _ -> "unknown"

  member _.InputPorts  = listPorts Input
  member _.OutputPorts = listPorts Output
  member x.TryFindInput (name: string)  = x.InputPorts  |> Array.tryFind (fun p -> p.Name = name)
  member x.TryFindOutput (name: string) = x.OutputPorts |> Array.tryFind (fun p -> p.Name = name)

  /// unopened input for a port (call `Open`)
  member _.GetInput (port: MidirPortInfo) =
    if port.Direction <> Input then invalidArg (nameof port) "not an input port"
    new MidirInput(port, trigger, openInput port)

  /// unopened output for a port (call `Open`)
  member _.GetOutput (port: MidirPortInfo) =
    if port.Direction <> Output then invalidArg (nameof port) "not an output port"
    new MidirOutput(port, openOutput port)

  member _.SupportsVirtualPorts = Native.mmidir_supports_virtual_ports() = 1

  /// Creates (and opens) a virtual input other apps can write to. Throws on Windows.
  member x.CreateVirtualInput (name: string) =
    if not x.SupportsVirtualPorts then
      raise (PlatformNotSupportedException "midir cannot create virtual ports on this OS (WinMM); use loopMIDI or Windows MIDI Services loopback endpoints")
    let info = { Id = "virtual:" + name; Name = name; Direction = Input; IsVirtual = true }
    let opener cb ud =
      let mutable h = 0n
      let code = Native.withUtf8 name (fun n l -> Native.mmidir_input_create_virtual(n, l, cb, ud, &&h))
      Check.ensure code (sprintf "create virtual input %s" name)
      h
    let input = new MidirInput(info, trigger, opener)
    input.Open 0
    input

  /// Creates (and opens) a virtual output other apps can read from. Throws on Windows.
  member x.CreateVirtualOutput (name: string) =
    if not x.SupportsVirtualPorts then
      raise (PlatformNotSupportedException "midir cannot create virtual ports on this OS (WinMM); use loopMIDI or Windows MIDI Services loopback endpoints")
    let info = { Id = "virtual:" + name; Name = name; Direction = Output; IsVirtual = true }
    let opener () =
      let mutable h = 0n
      let code = Native.withUtf8 name (fun n l -> Native.mmidir_output_create_virtual(n, l, &&h))
      Check.ensure code (sprintf "create virtual output %s" name)
      h
    let output = new MidirOutput(info, opener)
    output.Open()
    output

  interface IMidiPlatform<int64> with
    member x.InputDevices  = x.InputPorts  |> Array.map (fun p -> p :> IDeviceInfo)
    member x.OutputDevices = x.OutputPorts |> Array.map (fun p -> p :> IDeviceInfo)
    member x.GetMidiInput device =
      match device with
      | :? MidirPortInfo as p when p.Direction = Input && not p.IsVirtual -> Some (x.GetInput p :> IMidiInput<_>)
      | _ -> None
    member x.GetMidiOutput device =
      match device with
      | :? MidirPortInfo as p when p.Direction = Output && not p.IsVirtual -> Some (x.GetOutput p :> IMidiOutput<_>)
      | _ -> None

  interface IVirtualMidiPlatform<int64> with
    member x.SupportsVirtualPorts = x.SupportsVirtualPorts
    member x.CreateVirtualInput name = x.CreateVirtualInput name :> _
    member x.CreateVirtualOutput name = x.CreateVirtualOutput name :> _
