namespace Midi

module MessageMatching =

  type ProgramChange = {
    channel: byte
    program: byte
    other:  byte
  }
  let (|NoteOn|_|) (midiMessage: MidiMessage) =
    if midiMessage.MessageType = MidiMessageType.NoteOn then
      Some (midiMessage.Channel.Value, (*note*)midiMessage.Data1, (*velocity*)midiMessage.Data2)
    else
      None

  let (|ProgramChange|_|) (midiMessage: MidiMessage) =
    if midiMessage.MessageType = MidiMessageType.ProgramChange then
      Some {channel = midiMessage.Channel.Value; program = midiMessage.Data1; other = midiMessage.Data2 }
    else
      None

  let (|CC|_|) (channel: byte option) (ccNumber: byte) (midiMessage: MidiMessage) =
    if 
      midiMessage.MessageType = MidiMessageType.ControllerChange
      && (Option.isNone channel || midiMessage.Channel = channel)
      && midiMessage.Data1 = ccNumber
      then
      Some(midiMessage.Data2)
    else
      None
 