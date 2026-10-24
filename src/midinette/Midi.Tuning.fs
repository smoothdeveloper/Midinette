module midi.Tuning
open FSharp.UMX
open midinette.platform


module Internals =
  
  // adapted from github.com/surge-synthesizer/tuning-library
  
  type Tone =
    | ToneRatio of top: uint * bottom: uint
    | ToneCents of cents: decimal<cent>

  module Tone =
    let fromString (toneString: string) =
      let inline parseUint (toParse:string) = System.UInt32.TryParse toParse 
      if toneString.Contains "." then
        let inline parseDecimal (toParse:string) = System.Decimal.TryParse toParse 
        match parseDecimal toneString with
        | true, tone -> tone |> UMX.tag_cent |> ToneCents |> Some
        | _ -> None
      elif toneString.Contains "/" then
        match toneString.Split '/' with
        | [|top;bottom|] ->
          match parseUint top, parseUint bottom with
          | (true, top), (true, bottom) -> Some (ToneRatio(top, bottom))
          | _ -> None
        | _ -> None
      else
        match parseUint toneString with
        | true, top -> Some (ToneRatio(top, 1u))
        | _ -> None

    let centsForTone tone =
      match tone with
      | ToneCents cents -> cents
      | ToneRatio(top,0u) -> failwith $"tone ratio with 0 as divisor %A{top}"
      | ToneRatio(top,bottom) ->
        // 2^(cents/1200) = n/d
        // cents = 1200 * log(n/d) / log(2)
        let cents = 1200. * log (float (top / bottom)) / log (2.0)
        let cents = decimal cents
        LanguagePrimitives.DecimalWithMeasure<cent> cents
      
    let floatValue tone =
      let cents = centsForTone tone
      (UMX.untag cents / 1200m) + 1m
  /// The Scale is the representation of the SCL file. It contains several key
  /// features. Most importantly it has a count and a vector of Tones.
  ///
  /// In most normal use, you will simply pass around instances of this class
  /// to a Tunings::Tuning instance, but in some cases you may want to create
  /// or inspect this class yourself. Especially if you are displaying this
  /// class to your end users, you may want to use the rawText or count methods.
  type Scale =
    {
      /// The name in the SCL file. Informational only
      name: string
      /// The description in the SCL file. Informational only
      description: string
      /// The raw text of the SCL file used to create this Scale
      rawText: string
      /// The number of tones.
      count: int
      /// The tones
      tones: Tone array
    }
    static member atMidiKey (midiNote: MidiNote) scale =
      // API note: this is unsafe since Scale.tones can currently be initialized arbitrarily.
      scale.tones.[int midiNote]
  
  /// The KeyboardMapping class represents a KBM file. In most cases, the salient
  /// features are the tuningConstantNote and tuningFrequency, which allow you to
  /// pick a fixed note in the midi keyboard when retuning. The KBM file can also
  /// remap individual keys to individual points in a scale, which here is done with the
  /// keys vector.
  ///
  /// Just as with Scale, the rawText member contains the text of the KBM file used.
  type KeyboardMapping = {
    count              : int
    firstMidi          : int
    lastMidi           : int
    middleNote         : int
    tuningConstantNote : int
    tuningFrequency    : decimal
    tuningPitch        : decimal
    keys               : int array
    rawText            : string
    name               : string
  }
//  type Tuning = Tuning of int

  module SCLParser =
    type SCLItem =
      | Comment of comment: string
      | ScaleName of scaleName: string
      | ToneCount of toneCount: byte
      | ToneAsCent of cents: decimal
      | ToneAsRatio of top: uint * bottom: uint
      
    type ParserResult =
      | Parsed of items: SCLItem array
      
    let parse (reader: System.IO.TextReader) =
      let mutable line = reader.ReadLine()
      let mutable lineIndex = 0
      
      let mutable toneCountLine = None
      let mutable scaleNameLine = None
      [|
      while not (isNull line) do
        if System.String.IsNullOrWhiteSpace line then
          ()
        elif line.StartsWith "!" then
          Comment (line.Substring(1))
        else
          match scaleNameLine with
          | None ->
            printfn $"scale name: {scaleNameLine}"
            scaleNameLine <- Some (lineIndex, line)
            ScaleName line
          | Some _ ->
            let invariantCulture = System.Globalization.CultureInfo.InvariantCulture
            match toneCountLine with
            | None ->
              match System.Byte.TryParse(line, System.Globalization.NumberStyles.Number, invariantCulture) with
              | true, toneCount ->
                toneCountLine <- Some(lineIndex, toneCount)
                ToneCount toneCount
              | false, _ -> failwith $"don't know what to do of '{scaleNameLine}, {line}'"
            | Some _ ->
              match System.Decimal.TryParse(line, System.Globalization.NumberStyles.Number, invariantCulture) with
              | true, value ->
                ToneAsCent value
              | false, _ ->
                match line.Split '/' with
                | [|top;bottom|] ->
                  match System.UInt32.TryParse top, System.UInt32.TryParse bottom with
                  | (true, top), (true, bottom) ->
                    ToneAsRatio(top, bottom)
                    | _ -> failwith $"don't know what to do of '{line}'"
                | _ -> failwith $"don't know what to do of '{line}'"
      
        line <- reader.ReadLine()
        lineIndex <- lineIndex + 1
        
      |]




// based on specs from midi.org
// additional spec: http://www.somascape.org/midi/tech/spec.html#usx7E0800
type MessageId =
  | DumpRequestId            = 0uy
  | DumpResponseId           = 1uy
  | SingleNoteTuningChangeId = 2uy

let makeSysex deviceId (messageId: MessageId) data =
  let requiresCheckSum = 
    if messageId = MessageId.DumpResponseId then true
    else false

  let bytesForChecksum = 
    [|
      [|
        0x7euy
        deviceId
        0x08uy
        byte messageId
      |]
      data
    |]
    |> Array.concat

  let checkSum =
    if requiresCheckSum then 
      let mutable sum = 0uy 
      for b in bytesForChecksum do
        sum <- sum ^^^ b
      [|sum|]
    else
      [||]

  [|
    [|
      0xf0uy
    |]
    bytesForChecksum
    checkSum
    [|
      0xf7uy
    |]
  |]
  |> Array.concat
  |> UMX.tag_sysex_data

type NoteTuning =
  {
    semiTone: byte
    cents   : decimal
  }

type NoteTuningChange =
| NoFrequency
//| Hertz of hertz: decimal
| NoteAndCents of NoteTuning

type BulkTuningDumpResponse = 
  {
    name         : string
    programNumber: byte
    tunings      : NoteTuningChange array
  }

type TuningMessage =
| BulkTuningDumpRequest  of tuningProgramNumber: byte
| BulkTuningDump         of BulkTuningDumpResponse
| SingleNoteTuningChange of noteIndexAndTunings: (byte * NoteTuningChange) array
| TuningProgramSelect    of program: byte * channel: byte // + do RPNs inc/dec
with
  member x.IsRealTime =
    match x with
    | TuningProgramSelect _ | SingleNoteTuningChange _ -> true
    | BulkTuningDumpRequest _
    | BulkTuningDump _
      -> false

  member x.MessageId =
    match x with
    | BulkTuningDumpRequest _  -> Some MessageId.DumpRequestId
    | BulkTuningDump _         -> Some MessageId.DumpResponseId
    | SingleNoteTuningChange _ -> Some MessageId.SingleNoteTuningChangeId
    | TuningProgramSelect _    -> None

  member x.SysexData =
    let centIncrement = 0.0061m
    let centsAsShorts cents = int16 (cents / centIncrement)
    let centFirstByte centsAsShort = byte ((centsAsShort >>> 7) &&& 0b01111111s)
    let centSecondByte centsAsShort = byte (centsAsShort &&& 0b01111111s)
    match x with
    | BulkTuningDumpRequest programNumber  -> Some [|programNumber|]
    | BulkTuningDump {name=name; programNumber=programNumber; tunings=tunings} -> 
      Some 
        [|
          yield programNumber
          yield! (System.Text.Encoding.ASCII.GetBytes (name.PadLeft 16))
          for tuning in tunings do
            match tuning with
            | NoFrequency -> yield! [|0x7fuy; 0x7fuy; 0x7fuy |]
            | NoteAndCents(tuning) ->
              let centsAsShort = centsAsShorts tuning.cents
              let centFirstByte = centFirstByte centsAsShort
              let centSecondByte = centSecondByte centsAsShort
              yield! [|tuning.semiTone; centFirstByte; centSecondByte|]
        |]
    | SingleNoteTuningChange tunings -> 
      Some(
          [|
          for (noteIndex, tuning) in tunings do
            match tuning with
            | NoFrequency -> yield! [|noteIndex; 0x7fuy; 0x7fuy; 0x7fuy |]
            | NoteAndCents(tuning) ->
              let centsAsShort = centsAsShorts tuning.cents
              let centFirstByte = centFirstByte centsAsShort
              let centSecondByte = centSecondByte centsAsShort
              yield! [|noteIndex; tuning.semiTone; centFirstByte; centSecondByte|]
          |]
      )
    | TuningProgramSelect _    -> None

  member x.MakeMTSSysex deviceId =
    let sysex, messageId = x.SysexData, x.MessageId
    match sysex, messageId with
    | Some data, Some messageId ->
      Some (makeSysex deviceId messageId data)
    | _ -> None
  
  member x.Send (deviceId: byte, midiOutput: IMidiOutput<_>, nowTimestamp) =
    match x.MakeMTSSysex deviceId with
    | Some sysex -> midiOutput.WriteSysex (nowTimestamp())  sysex
    | None -> ()

    match x with
    | BulkTuningDumpRequest _ | BulkTuningDump _ | SingleNoteTuningChange _ -> ()
    | TuningProgramSelect _ ->
      //let rpn = 3
      //let messages = midi.Nrpn.makeNRPN2 1uy 1uy 1uy
      //midiOutput.WriteMessages (nowTimestamp()) messages
      ()
      (*

*)

