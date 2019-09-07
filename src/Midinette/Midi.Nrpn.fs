namespace Midi

open Midi.MessageMatching

module Nrpn =
  let makeNRPN1 hi value =
    [|
      MidiMessage.CC 0uy 99uy hi
      MidiMessage.CC 0uy 6uy value
    |]

  let makeNRPN2 hi lo value =
    [|
      MidiMessage.CC 0uy 99uy hi
      MidiMessage.CC 0uy 98uy lo
      MidiMessage.CC 0uy 6uy value
    |]

  let makeNRPN3 hi lo valueHi valueLo =
    [|
      MidiMessage.CC 0uy 99uy hi
      MidiMessage.CC 0uy 98uy lo
      MidiMessage.CC 0uy 6uy valueHi
      MidiMessage.CC 0uy 38uy valueLo
    |]

  type NRPNEvent =
  | NRPN of ((byte * byte) * (byte * byte))
  | NRPN7 of ((byte * byte) * (byte * byte) * (byte * byte))
  | NRPN14 of ((byte * byte) * (byte * byte) * (byte * byte) * (byte * byte))



  type NRPNChannelRegister<'timestamp>
    (channel: byte, isExpired: 'timestamp -> bool, zeroTimestamp: 'timestamp) =
     
    let paramNumMSB = 127uy
    let paramNumLSB = 127uy
    let mutable firstMessageTimestamp = zeroTimestamp
    let messages = ResizeArray<MidiEvent<'timestamp>> 4
    let nrpnEvent = Event<_>()
    let mutable subscription = Unchecked.defaultof<System.IDisposable>

    let clear() =
      firstMessageTimestamp <- zeroTimestamp
      messages.Clear()

    let push (startIndex: int) (endIndex: int) =
      let getBytes (e: MidiEvent<_>) = e.Message.Data1, e.Message.Data2
      let messages = [|
        for i in startIndex .. endIndex do
          yield messages.[i]
      |]
    
      let event = 
        match messages.Length with
        | 2 -> Some (NRPN(getBytes messages.[0], getBytes messages.[1]))
        | 3 -> Some (NRPN7(getBytes messages.[0], getBytes messages.[1], getBytes messages.[2]))
        | 4 -> Some (NRPN14(getBytes messages.[0], getBytes messages.[1], getBytes messages.[2], getBytes messages.[3]))
        | _ -> None
      match event with
      | Some event -> nrpnEvent.Trigger (messages.[messages.Length - 1].Timestamp, event)
      | _ -> ()

    let rec checkQueue (timestamp: 'timestamp) =
      if messages.Count >= 3 then
        let mutable startIndex = -1
        let mutable endIndex = -1
        let maybeMessage = [|
          let mutable lastData1 = 0uy
          let mutable count = 0
          let mutable notDone = true
          let mutable i = 0
          while notDone do
          //for i in 0 .. messages.Count - 1 do
            if i < messages.Count then
              let m = messages.[i].Message
              if m.Data1 = 99uy then
                startIndex <- i
                lastData1 <- m.Data1
                count <- count + 1
                yield messages.[i]
              if startIndex >= 0 && i > startIndex then
                match lastData1, m.Data1 with
                | 99uy,98uy | 98uy,6uy | 6uy, 38uy -> endIndex <- i; lastData1 <- m.Data1; count <- count + 1; yield messages.[i]
                | _ -> startIndex <- -1
              i <- i + 1
            if i >= messages.Count then
              notDone <- false
            elif count = 4 then
              notDone <- false
        |]
        
      
        if startIndex >= 0 && endIndex >= 0 then
          let count = (endIndex - startIndex) + 1
          if count = 3 || count = 4 then
            push startIndex endIndex
            messages.RemoveRange(0, endIndex + 1)
            checkQueue timestamp
          elif count > 0 && endIndex > 0 then
            messages.RemoveRange(0, endIndex + 1)
    
    let clearIfExpired (timestamp: 'timestamp) =
      if isExpired timestamp then
      //if (timestamp - firstMessageTimestamp) > timestampThreshold then
        checkQueue timestamp
        clear()

    let keepMessage (e: MidiEvent<_>) =
      if messages.Count = 0 then firstMessageTimestamp <- e.Timestamp
      messages.Add e
    
      // TODODODODODO
      (*
    do
      subscription <- Midi.Runtime.midiCallback.Publish.Subscribe(fun timestamp -> 
        clearIfExpired timestamp
        ()
      )*)

    interface System.IDisposable with
      member x.Dispose() = use ___ : System.IDisposable = subscription in ()
    [<CLIEvent>] member x.NRPN = nrpnEvent.Publish
  
    member x.NoticeMessage (e: MidiEvent<'timestamp>) =
    
      clearIfExpired (e.Timestamp)
      match e.Message with
      | CC (Some channel) 99uy ee -> 
        clear()
        keepMessage e
      | CC (Some channel) 98uy ee -> keepMessage e
      | CC (Some channel) 6uy ee -> keepMessage e
      | CC (Some channel) 38uy ee -> keepMessage e
      | _ -> clear()
