namespace Midi

module MessageMatching =

  type ProgramChange = {
    channel: byte_7bits
    program: byte_7bits
    other:  byte_7bits
  }
  type ControlChange = {
    channel: byte_7bits
    control: byte_7bits
    value: byte_7bits

  }
  let (|NoteOn|_|) (midiMessage: MidiMessage) =
    if midiMessage.MessageType = MidiMessageType.NoteOn then
      Some (midiMessage.Channel.Value, (*note*)midiMessage.Data1, (*velocity*)midiMessage.Data2)
    else
      None

  let (|NoteOff|_|) (midiMessage: MidiMessage) =
    if midiMessage.MessageType = MidiMessageType.NoteOff then
      Some (midiMessage.Channel.Value, (*note*)midiMessage.Data1, (*velocity*)midiMessage.Data2)
    else
      None

  let (|ProgramChange|_|) (midiMessage: MidiMessage) =
    if midiMessage.MessageType = MidiMessageType.ProgramChange then
      Some {channel = midiMessage.Channel.Value; program = UMX.tag_byte_7bits midiMessage.Data1; other = UMX.tag_byte_7bits midiMessage.Data2 }
    else
      None

  let (|CC|_|) (channel: byte option) (ccNumber: byte) (midiMessage: MidiMessage) =
    if 
      midiMessage.MessageType = MidiMessageType.ControllerChange
      && (Option.isNone channel || midiMessage.Channel = UMX.tag_byte_7bits channel)
      && midiMessage.Data1 = ccNumber
      then
      Some { channel = midiMessage.Channel.Value; control = UMX.tag_byte_7bits midiMessage.Data1; value = UMX.tag_byte_7bits midiMessage.Data2}
    else
      None
 