namespace Midi

open System
open System.Collections.Generic

module Registers =

  open Midi
  open System.Collections.Generic

  type MidiChannelState =
    val seenControllers         : HashSet<byte>
    val controllers             : byte array
    val activeNotes             : HashSet<byte>
    val notes                   : (byte * int) array
    //val mutable modWheel        : byte
    val mutable pitchBend       : int16
    val mutable channelPressure : byte
    val mutable program         : byte
  
    new() =
      {
        program         = 0uy
        controllers     = Array.zeroCreate 128
        seenControllers = HashSet<_>()
        activeNotes     = HashSet<_>()
        notes           = Array.zeroCreate 128
        pitchBend       = 8192s
        channelPressure = 0uy
      }
    member this.NoticeProgramChange program =
      this.program <- program
    member this.NoticeControllerChange cc value =
      this.seenControllers.Add cc |> ignore
      this.controllers.[int cc] <- value
    member this.NoticeNoteOn note velocity timestamp =
      if velocity = 0uy then this.NoticeNoteOff note velocity timestamp else
      this.activeNotes.Add note |> ignore
      this.notes.[int note] <- velocity, timestamp
    member this.NoticeNoteOff note velocity timestamp =
      this.activeNotes.Remove note |> ignore
      this.notes.[int note] <- velocity, timestamp

    member this.UpdateWithEvent (m: MidiEvent) =
      let message = m.Message
      let channelState = this
      match message.MessageType with
      | MidiMessageType.ProgramChange    -> channelState.program <- message.Data1
      | MidiMessageType.ChannelPressure  -> channelState.channelPressure <- message.Data1
      | MidiMessageType.ControllerChange -> channelState.NoticeControllerChange message.Data1 message.Data2
      | MidiMessageType.PitchBendChange  -> channelState.pitchBend <- ((int16 message.Data2) <<< 7) ||| (int16 message.Data1)
      | MidiMessageType.NoteOn           -> channelState.NoticeNoteOn message.Data1 message.Data2 m.Timestamp
      | MidiMessageType.NoteOff          -> channelState.NoticeNoteOff message.Data1 message.Data2 m.Timestamp
      | _ -> ()

type CircularBuffer<'t> =
    // https://github.com/joaoportela/CircullarBuffer-CSharp/blob/master/CircularBuffer/CircularBuffer.cs
    val items              : 't array
    val mutable startIndex : int
    val mutable endIndex   : int
    val mutable size       : int
    
    new(capacity) =
      {
        items = Array.zeroCreate capacity
        startIndex = 0
        endIndex = 0
        size = 0
      }
    new(capacity, items: 't array) =
      {
        items = Array.init capacity (fun i -> if i < items.Length then items.[i] else Unchecked.defaultof<'t>)
        startIndex = 0
        size = items.Length
        endIndex = if items.Length = capacity then 0 else items.Length
      }
    
    member private x.throwIfEmpty () =
      if x.IsEmpty then failwith "empty"

    member private x.increment(v:byref<int>) =
      if v + 1 = x.Capacity then
        v <- 0
    member private x.decrement(v: byref<int>) =
      if v = 0 then
        v <- x.Capacity
      v <- v - 1
    member private x.arrayOne =
      if x.startIndex < x.endIndex then
        ArraySegment(x.items, x.startIndex, x.endIndex - x.startIndex)
      else
        ArraySegment(x.items, x.startIndex, x.items.Length - x.startIndex)
    member private x.arrayTwo =
      if x.startIndex < x.endIndex then
        ArraySegment(x.items, x.endIndex, 0)
      else
        ArraySegment(x.items, 0, x.endIndex)
    member x.Capacity = x.items.Length
    member x.IsFull = x.size = x.Capacity
    member x.IsEmpty = x.size = 0
    member x.Front =
      x.throwIfEmpty ()
      x.items.[x.startIndex]
    member x.Back =
      x.throwIfEmpty ()
      x.items.[(if x.endIndex <> 0 then x.endIndex else x.Capacity) - 1]
    member x.PushBack item =
      if x.IsFull then
        x.items.[x.endIndex] <- item
        x.increment (ref x.endIndex)
        x.startIndex <- x.endIndex
      else
        x.items.[x.endIndex] <- item
        x.increment (ref x.endIndex)
        x.size <- x.size + 1
    member x.PushFront item =
      if x.IsFull then
        x.decrement (ref x.endIndex)
        x.endIndex <- x.startIndex
        x.items.[x.startIndex] <- item
      else
        x.decrement (ref x.endIndex)
        x.items.[x.startIndex] <- item
        x.size <- x.size + 1
    member x.PopBack () =
      x.throwIfEmpty ()
      x.decrement (ref x.endIndex)
      x.items.[x.endIndex] <- Unchecked.defaultof<'t>
      x.size <- x.size - 1
    member x.PopFront () =
      x.throwIfEmpty ()
      x.items.[x.startIndex] <- Unchecked.defaultof<'t>
      x.increment (ref x.startIndex)
      x.size <- x.size - 1
    
    member x.ToArray () =
      let newArray = Array.zeroCreate x.size
      let mutable newArrayOffset = 0
      let segments = [|x.arrayOne; x.arrayTwo|]
      for segment in segments do
        System.Array.Copy(segment.Array, segment.Offset, newArray, newArrayOffset, segment.Count)
        newArrayOffset <- newArrayOffset + segment.Count
      newArray
    (*
    interface IEnumerable<'t> with
      member x.GetEnumerator () : IEnumerator<'t> =
        seq {
          let segments = [|x.arrayOne; x.arrayTwo|]
          for segment in segments do
            for i in 0 .. (segment.Count - 1) do
              yield segment.Array.[segment.Offset + i]
        }*)

type MidiRealtimeState =

  val mutable started : bool

  new() =
    {
      started = false
    }
  member x.IsStarted = x.started
  member this.UpdateWithEvent (m: MidiEvent) =
    let message = m.Message
    match message.MessageType with
    | MidiMessageType.TimingClock -> () //m.Timestamp
    | MidiMessageType.Start    -> this.started <- true
    | MidiMessageType.Continue -> this.started <- true
    | MidiMessageType.Stop     -> this.started <- false
    | _ -> ()