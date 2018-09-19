namespace Midi

type MidiNote =
| C      = 0x0uy
| CSharp = 0x1uy
| D      = 0x2uy
| DSharp = 0x3uy
| E      = 0x4uy
| F      = 0x5uy
| FSharp = 0x6uy
| G      = 0x7uy
| GSharp = 0x8uy
| A      = 0x9uy
| ASharp = 0xauy
| B      = 0xbuy

// https://www.midi.org/specifications-old/item/table-1-summary-of-midi-message
type MidiMessageType =
/// This message is sent when a note is released (ended). (kkkkkkk) is the key (note) number. (vvvvvvv) is the velocity.
| NoteOff                  = 0x80uy
/// This message is sent when a note is depressed (start). (kkkkkkk) is the key (note) number. (vvvvvvv) is the velocity.
| NoteOn                   = 0x90uy
/// This message is most often sent by pressing down on the key after it "bottoms out". (kkkkkkk) is the key (note) number. (vvvvvvv) is the pressure value.
| PolyKeyPressure          = 0xa0uy
/// This message is sent when a controller value changes. Controllers include devices such as pedals and levers. Controller numbers 120-127 are reserved as "Channel Mode Messages" (below). (ccccccc) is the controller number (0-119). (vvvvvvv) is the controller value (0-127).
| ControllerChange         = 0xb0uy
/// This message sent when the patch number changes. (ppppppp) is the new program number.
| ProgramChange            = 0xc0uy
/// This message is most often sent by pressing down on the key after it "bottoms out". This message is different from polyphonic after-touch. Use this message to send the single greatest pressure value (of all the current depressed keys). (vvvvvvv) is the pressure value.
| ChannelPressure          = 0xd0uy
/// Pitch Bend Change. This message is sent to indicate a change in the pitch bender (wheel or lever, typically). The pitch bender is measured by a fourteen bit value. Center (no pitch change) is 2000H. Sensitivity is a function of the receiver, but may be set using RPN 0. (lllllll) are the least significant 7 bits. (mmmmmmm) are the most significant 7 bits.
| PitchBendChange          = 0xe0uy
/// 
| SysEx                    = 240uy
/// MIDI Time Code Quarter Frame. nnn = Message Type dddd = Values
| MidiTimeCodeQuarterFrame = 241uy
/// This is an internal 14 bit register that holds the number of MIDI beats (1 beat= six MIDI clocks) since the start of the song. l is the LSB, m the MSB.
| SongPositionPointer      = 242uy
/// The Song Select specifies which sequence or song is to be played.
| SongSelect               = 243uy
/// Tune Request. Upon receiving a Tune Request, all analog synthesizers should tune their oscillators.
| TuneRequest              = 246uy
/// End of Exclusive. Used to terminate a System Exclusive dump (see above).
| SysExEnd                 = 247uy
/// Timing Clock. Sent 24 times per quarter note when synchronization is required (see text).
| TimingClock              = 248uy
/// Start. Start the current sequence playing. (This message will be followed with Timing Clocks).
| Start                    = 250uy
/// Continue. Continue at the point the sequence was Stopped.
| Continue                 = 251uy
/// Stop. Stop the current sequence.
| Stop                     = 252uy
/// Active Sensing. This message is intended to be sent repeatedly to tell the receiver that a connection is alive. Use of this message is optional. When initially received, the receiver will expect to receive another Active Sensing message each 300ms (max), and if it does not then it will assume that the connection has been terminated. At termination, the receiver will turn off all voices and return to normal (non- active sensing) operation. 
| ActiveSensing            = 254uy
/// Reset. Reset all receivers in the system to power-up status. This should be used sparingly, preferably under manual control. In particular, it should not be sent on power-up.
| SystemReset              = 255uy


module MidiMessageTypeIdentifaction =
  let inline isRealtimeMessage messageType = messageType >= MidiMessageType.TimingClock && messageType <= MidiMessageType.SystemReset
  let inline isSystemMessage messageType = messageType >= MidiMessageType.SysEx && messageType <= MidiMessageType.SysExEnd
  let inline isChannelMessage messageType = messageType >= MidiMessageType.NoteOff && messageType <= (LanguagePrimitives.EnumOfValue 239uy)
  let inline isSysexBeginOrEnd messageType = messageType = MidiMessageType.SysEx || messageType = MidiMessageType.SysExEnd

open MidiMessageTypeIdentifaction

type [<Struct>] MidiMessage private(value:int) =
  static member StatusWithChannel (messageType: MidiMessageType) channel = byte messageType + channel
  static member NoteWithOctave (note: MidiNote) (octave: byte) = (octave * 12uy) + (byte note)
  static member GetNoteAndOctave (midiNoteNumber: byte) =
    let octave = midiNoteNumber / 12uy
    let note : MidiNote = midiNoteNumber % 12uy |> LanguagePrimitives.EnumOfValue
    note, octave

  static member Encode (status: byte) (data1: byte) (data2: byte) =
    MidiMessage( 
      (((int data2) <<< 16) &&& 0xff0000)
      ||| (((int data1) <<< 8) &&& 0xff00)
      ||| ((int status) &&& 0xff)
    )

  static member EncodeChannelMessage (messageType: MidiMessageType) (channel: byte) (data1: byte) (data2: byte) =
    MidiMessage.Encode (MidiMessage.StatusWithChannel messageType channel) data1 data2
  static member NoteOn channel note velocity  = MidiMessage.EncodeChannelMessage MidiMessageType.NoteOn channel note velocity
  static member NoteOff channel note velocity = MidiMessage.EncodeChannelMessage MidiMessageType.NoteOff channel note velocity
  static member ProgramChange channel program = MidiMessage.EncodeChannelMessage MidiMessageType.ProgramChange channel program 0uy
  static member CC channel control value      = MidiMessage.EncodeChannelMessage MidiMessageType.ControllerChange channel control value
  static member FromWord word = MidiMessage word
  member x.Word = value
  member x.Status = byte (value &&& 0xff)
  member x.Data1 = byte ((value >>> 8) &&& 0xff)
  member x.Data2 = byte ((value >>> 16) &&& 0xff)
  member x.IsChannelMessage = (x.Status |> LanguagePrimitives.EnumOfValue) |> isChannelMessage
  member x.MessageType : MidiMessageType =
    let messageType =
      if x.IsChannelMessage then
        (x.Status &&& 0b11110000uy)
      else
        x.Status
    messageType |> LanguagePrimitives.EnumOfValue
    
  member x.Channel =
    if x.IsChannelMessage then
      Some (x.Status &&& 0b00001111uy)
    else
      None
  override x.ToString () = 
    if x.IsChannelMessage then
      match x.MessageType with
      | MidiMessageType.NoteOn | MidiMessageType.NoteOff ->
        let note, octave = MidiMessage.GetNoteAndOctave x.Data1
        let noteName =
          match note with
          | MidiNote.A | MidiNote.B | MidiNote.C | MidiNote.D | MidiNote.E | MidiNote.F | MidiNote.G -> note.ToString()
          | MidiNote.ASharp -> "A#"
          | MidiNote.CSharp -> "C#"
          | MidiNote.DSharp -> "D#"
          | MidiNote.FSharp -> "F#"
          | MidiNote.GSharp -> "G#"
          | _ -> failwithf "note %i" (byte note)
            
        sprintf "%20s (channel:%02i) note (%03i): %2s octave: %i velocity %i" (string x.MessageType) x.Channel.Value x.Data1 noteName octave x.Data2
      | _ ->
        sprintf "%20s (channel:%02i) %03i %03i" (string x.MessageType) x.Channel.Value x.Data1 x.Data2
    else
      sprintf "%A %i %i" ((x.Status |> LanguagePrimitives.EnumOfValue): MidiMessageType) x.Data1 x.Data2

type [<Struct>] MidiEvent<'timestamp> (message: MidiMessage, timestamp: 'timestamp) =
  member __.Message = message
  member __.Timestamp = timestamp

type IDeviceInfo =
  abstract member Name     : string
  abstract member DeviceId : int

type IMidiInput<'timestamp> =
  [<CLIEvent>] abstract member Error : IEvent<string>
  [<CLIEvent>] abstract member ChannelMessageReceived : IEvent<MidiEvent<'timestamp>>
  [<CLIEvent>] abstract member SystemMessageReceived : IEvent<MidiEvent<'timestamp>>
  [<CLIEvent>] abstract member SysexReceived : IEvent<byte array>
  [<CLIEvent>] abstract member RealtimeMessageReceived : IEvent<MidiEvent<'timestamp>>
  abstract member Open: bufferSize:int -> unit
  abstract member Close: unit -> unit
  abstract member DeviceInfo : IDeviceInfo

type IMidiOutput<'timestamp> =
  abstract member WriteMessage: timestamp:'timestamp -> midiMessage:MidiMessage -> unit
  abstract member WriteMessages: timestamp:'timestamp -> midiMessages:MidiMessage array -> unit
  abstract member WriteSysex: timestamp:'timestamp -> data:byte array -> unit
  abstract member Open: bufferSize:int -> latency: int -> unit
  abstract member Close: unit -> unit
  abstract member DeviceInfo : IDeviceInfo
(*
module MidiProgram =
  let change channel program (output: IMidiOutput<'timestamp>) =
    MidiMessage.EncodeChannelMessage MidiMessageType.ProgramChange channel program 0uy
    |> output.WriteMessage 0*)