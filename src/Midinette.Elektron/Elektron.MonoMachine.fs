module Elektron.MonoMachine

open System
open Midi
open Elektron.Platform
open Elektron.Platform.SilverMachines
type midi7 = byte
type midi8 = byte
type LFOShape =
  | Tri    | InvertedTri 
  | Saw    | InvertedSaw
  | Square | InvertedSquare
  | Exp    | InvertedExp
  | Ramp   | InvertedRamp
  | Random
 with
  static member FromCCValue (value: midi7) =
    match value with
    | _ -> failwithf "%i not defined" value
    
    
type LFOMultiplier = Times1 | Times2 | Times4 | Times8 | Times16 | Times32 | Times64
  
module internal Helpers =
  let monoMachineHeader = [|
      0xf0uy
      0x00uy
      0x20uy
      0x3cuy
      0x03uy
      0x00uy
    |]

  let makeSysexMessage message = 
    let sysexEnd = [|0xf7uy|] 
    Array.concat [|monoMachineHeader;message;sysexEnd|]
    
open Helpers
open System.Text
open System.Security.Authentication.ExtendedProtection

type MonoMachineStatusType =
| GlobalSlot
| KitNumber
| PatternNumber
| SongNumber
| SequencerMode
| AudioMode
| SequencerModeMode
with
  member x.Id =
    match x with
    | GlobalSlot        -> 0x01uy
    | KitNumber         -> 0x02uy
    | PatternNumber     -> 0x04uy
    | SongNumber        -> 0x08uy
    | SequencerMode     -> 0x10uy
    | AudioMode         -> 0x20uy
    | SequencerModeMode -> 0x21uy
  static member FromByte v =
    match v with
    | 0x01uy -> GlobalSlot
    | 0x02uy -> KitNumber
    | 0x04uy -> PatternNumber
    | 0x08uy -> SongNumber
    | 0x10uy -> SequencerMode
    | 0x20uy -> AudioMode
    | 0x21uy -> SequencerModeMode
    | v      -> failwithf "unknown status type %i" v


type Track = Track1 | Track2 | Track3 | Track4 | Track5 | Track6
with
  static member FromByte b =
    match b with
    | 0x0uy -> Track1
    | 0x1uy -> Track2
    | 0x2uy -> Track3
    | 0x3uy -> Track4
    | 0x4uy -> Track5
    | 0x5uy -> Track6
    | _ -> failwithf "track %i unknown" b

  static member ToByte t =
    match t with
     | Track1 -> 0x0uy 
     | Track2 -> 0x1uy 
     | Track3 -> 0x2uy 
     | Track4 -> 0x3uy 
     | Track5 -> 0x4uy 
     | Track6 -> 0x5uy

type AssignMachineParameterMode =
| DoNotAssignParameters
| InitAllDataPages
| InitSynthesisPages
with
  member x.Value =
    match x with
    | DoNotAssignParameters -> 0x0uy
    | InitAllDataPages -> 0x1uy
    | InitSynthesisPages -> 0x2uy
type MonoMachineType =
| GND_GND   | SWAVE_SAW | SID_6581
| GND_SIN   | SWAVE_PULS
| GND_NOIS  | SWAVE_ENS 
| DPRO_WAVE | FM_STAT   | VO_VO6
| DPRO_BBOX | FM_PAR    
| DPRO_DDRW | FM_DYN    
| DPRO_DENS 
| FX_THRU   
| FX_REVERB 
| FX_CHORUS 
| FX_DYNAMIX
| FX_RINGMOD
| FX_PHASER 
| FX_FLANGER
with
  static member FromValue =
    function
    | 0x00uy -> GND_GND    | 0x04uy -> SWAVE_SAW  | 0x03uy -> SID_6581
    | 0x01uy -> GND_SIN    | 0x05uy -> SWAVE_PULS 
    | 0x02uy -> GND_NOIS   | 0x14uy -> SWAVE_ENS  
    | 0x06uy -> DPRO_WAVE  | 0x08uy -> FM_STAT    | 0x11uy -> VO_VO6
    | 0x07uy -> DPRO_BBOX  | 0x09uy -> FM_PAR     
    | 0x32uy -> DPRO_DDRW  | 0x10uy -> FM_DYN     
    | 0x33uy -> DPRO_DENS 
    | 0x12uy -> FX_THRU   
    | 0x13uy -> FX_REVERB 
    | 0x15uy -> FX_CHORUS 
    | 0x16uy -> FX_DYNAMIX
    | 0x17uy -> FX_RINGMOD
    | 0x18uy -> FX_PHASER 
    | 0x19uy -> FX_FLANGER
    | x -> failwithf "unknown machine type %x" x
  member x.Value =
    match x with
    | GND_GND    -> 0x00uy | SWAVE_SAW  -> 0x04uy | SID_6581 -> 0x03uy
    | GND_SIN    -> 0x01uy | SWAVE_PULS -> 0x05uy
    | GND_NOIS   -> 0x02uy | SWAVE_ENS  -> 0x14uy
    | DPRO_WAVE  -> 0x06uy | FM_STAT    -> 0x08uy | VO_VO6 -> 0x11uy
    | DPRO_BBOX  -> 0x07uy | FM_PAR     -> 0x09uy
    | DPRO_DDRW  -> 0x32uy | FM_DYN     -> 0x10uy
    | DPRO_DENS  -> 0x33uy
    | FX_THRU    -> 0x12uy
    | FX_REVERB  -> 0x13uy
    | FX_CHORUS  -> 0x15uy
    | FX_DYNAMIX -> 0x16uy
    | FX_RINGMOD -> 0x17uy
    | FX_PHASER  -> 0x18uy
    | FX_FLANGER -> 0x19uy

type MachineParameters(bytes: byte array) =
  let synthesisBase = 0x0
  let ampBase = 0x8
  let filterBase = 0x10
  let effectBase = 0x18
  let lfo1Base = 0x20
  let lfo2Base = 0x28
  let lfo3Base = 0x30
  let midiBase = 0x38
  let adsrBase = 0x40
  let getBytes baseAddress = bytes.[baseAddress .. baseAddress + 7]

  member x.Synthesis = getBytes synthesisBase
  member x.Amp       = getBytes ampBase
  member x.Filter    = getBytes filterBase
  member x.Effects   = getBytes effectBase
  member x.LFO1      = getBytes lfo1Base
  member x.LFO2      = getBytes lfo2Base
  member x.LFO3      = getBytes lfo3Base
  member x.MIDI      = getBytes midiBase
  member x.ADSR      = getBytes adsrBase

type MonoMachineKit (data: byte array) =
  member x.Version          = data.[0x07]
  member x.Revision         = data.[0x08]
  member x.OriginalPosition = data.[0x09]
  member x.MessageLength = getLengthFromSysexMessage data
  member x.CheckSum = getCheckSumFromSysexMessage data
  member x.unpacked =
    if areMonoMachineCheckSumAndLengthValid data then
      getMonoMachineDataSliceFromSysexMessage data
      |> Seq.toArray
      |> dataToByte
    else [||]
  member x.Name =
    x.unpacked |> getSlice 0 11 |> ASCIIEncoding.ASCII.GetString
  member x.Levels =
    x.unpacked |> getSlice 0xb 6
  member x.Parameters =
    x.unpacked 
    |> getSlice 0x11 (72 * 6) 
    |> Array.chunkBySize 72 
    |> Array.map MachineParameters
  member x.Machines =
    x.unpacked
    |> getSlice 0x1c1 6
    |> Array.map ((&&&) 0b01111111uy)
    |> Array.map MonoMachineType.FromValue

type VelocityCurve = Linear | Soft | Fixed of value: byte
with
  static member FromByte v =
    match v with
    | 0uy -> Linear
    | 1uy -> Soft
    | _ -> Fixed v
  static member ToByte v =
    match v with
    | Linear  -> 0uy
    | Soft    -> 1uy
    | Fixed v -> v

type GlobalSettings = {
  midiChannelAutoTrack    : byte
  midiChannel             : byte
  midiChannelMultiTrig    : byte
  midiChannelMultiMap     : byte
  midiMachineChannels     : byte array
  ccDestinationsPerChannel: byte array array
  programChangeIn         : bool
  velocityCurve           : VelocityCurve
  fixedVelocity           : byte
  knobSpeed               : byte
  baseFrequency           : int
}
with
  static member FromSysex (sysexData: byte array) =
    if areMonoMachineCheckSumAndLengthValid sysexData then
      let globalData = (getMonoMachineDataSliceFromSysexMessage sysexData) |> Seq.toArray |> Elektron.Platform.byteToData
      if globalData.Length < 0x101 then None
      else
        Some
          {
          midiChannelAutoTrack     = globalData.[0x00]
          midiChannel              = globalData.[0x01]
          midiChannelMultiTrig     = globalData.[0x02]
          midiChannelMultiMap      = globalData.[0x03]
          midiMachineChannels      = [||]//globalData.[0x12..0x1]

          // external sync
          ccDestinationsPerChannel = [||]//[|globalData.[0x18..0x1]|]
          programChangeIn          = globalData.[0xfd] = 1uy
          velocityCurve            = globalData.[0xff] |> VelocityCurve.FromByte
          fixedVelocity            = globalData.[0x100]
          knobSpeed                = globalData.[0x101]
          baseFrequency            = 0 //globalData.[0x104..] |> Elektron.fourBytesToBigEndianInt
          }
    else
      printf "can't parse globalsettings from sysex %A" sysexData
      None

type MonoMachineSysexResponse =
| GlobalSettings of GlobalSettings
| Kit of MonoMachineKit
| Pattern of byte array
| Song of byte array
| StatusResponse of statusType: MonoMachineStatusType * value: byte
with
  static member BuildResponse (bytes: byte array) =
    match bytes.[6] with
    | 72uy -> StatusResponse(MonoMachineStatusType.FromByte bytes.[7], bytes.[8])
    | _ -> failwithf "h:%x response not understood" bytes.[6]


type MonoMachineSysexRequests =
| DumpGlobalSettings of globalSettingsIndex: byte
| DumpKit of kit: byte
| DumpPattern of pattern: byte
| DumpSong of song: byte
| QueryStatus of statusType: MonoMachineStatusType
| AssignMachine of track: byte * machine: MonoMachineType * parametersMode: AssignMachineParameterMode
with
  member x.MessageId = 
    match x with
    | DumpGlobalSettings _ -> 0x51uy
    | DumpKit _            -> 0x53uy
    | AssignMachine _      -> 0x5buy
    | DumpPattern _        -> 0x68uy
    | DumpSong _           -> 0x6auy
    | QueryStatus _        -> 0x70uy
  member x.Sysex =
    let data =
      match x with
      | DumpGlobalSettings id -> id |> Array.singleton
      | DumpKit kit           -> kit |> Array.singleton
      | DumpPattern pattern   -> pattern |> Array.singleton
      | DumpSong song         -> song |> Array.singleton
      | QueryStatus status    -> status.Id |> Array.singleton
      | AssignMachine (t,m,p) -> [|t;m.Value;p.Value|]
    makeSysexMessage (Array.concat ([|x.MessageId |> Array.singleton; data|]))
  member x.ResponseMessageId =
    match x with
    | DumpGlobalSettings _ 
    | DumpKit _            
    | DumpPattern _        
    | DumpSong _           -> Some (x.MessageId - 1uy)
    | AssignMachine _      -> None
    | QueryStatus _        -> Some 0x72uy
  member x.BuildResponse data =
    match x with
    | DumpGlobalSettings _ -> GlobalSettings.FromSysex data |> Option.map GlobalSettings
    | DumpKit _            -> Some <| Kit(MonoMachineKit data)
    | DumpPattern _        -> Some <| Pattern data
    | DumpSong _           -> Some <| Song data
    | QueryStatus _        -> Some <| StatusResponse((MonoMachineStatusType.FromByte data.[monoMachineHeader.Length + 1]), data.[monoMachineHeader.Length + 2])
    | AssignMachine(_)     -> failwithf "not implemented"
    //| _ -> None

type MonoMachineTrig =
| Amp    = 0b0001uy
| Filter = 0b010uy
| LFO    = 0b0100uy


type MonoMachine(inPort: IMidiInput<_>, outPort: IMidiOutput<_>) =
  let helpGetMonomachineSysex maxMessage (timeout: TimeSpan) (request: MonoMachineSysexRequests) (inPort: IMidiInput<_>) =
#if FABLE_COMPILER
    failwithf "TODO FABLE"
#else
    let getSysexAsync =
      Midi.Sysex.helpGetSysex 
          maxMessage 
          timeout 
          (fun sysex -> 
              sysex.[0..5] = Helpers.monoMachineHeader 
              && Some sysex.[6] = request.ResponseMessageId
          )
          request.BuildResponse
          inPort
    async {
      let! sysx = getSysexAsync
      match sysx with
      | Choice1Of2(Some(sysx)) -> return sysx
      | Choice1Of2 (_) -> 
          printfn "oops1"
          return None
      | Choice2Of2 exn -> 
        printfn "oops2"
        return None
    }
#endif
  let performSysExRequest (requestMessage: MonoMachineSysexRequests) =
    match requestMessage.ResponseMessageId with
    | Some id ->
      let task = 
        helpGetMonomachineSysex 5 (TimeSpan.FromMilliseconds(5000.)) requestMessage inPort
        |> Async.StartAsTask
      requestMessage.Sysex |> outPort.WriteSysex 0
      task.Result
    | None ->
      requestMessage.Sysex |> outPort.WriteSysex 0
      None

  member x.MidiOutPort = inPort
  member x.MidiInPort = outPort

  member x.ChangeLevel track level =
    Midi.Nrpn.makeNRPN2 (00uy + track) 127uy level
    |> outPort.WriteMessages 0
  member x.ChangePitch track pitch =
    Midi.Nrpn.makeNRPN1 (112uy + track) pitch
    |> outPort.WriteMessages 0
  member x.SendTrigs track (trigs: MonoMachineTrig) =
    Midi.Nrpn.makeNRPN1 127uy ((track <<< 4) + (byte trigs))
    |> outPort.WriteMessages 0
  member x.DumpGlobal id =
    performSysExRequest (DumpGlobalSettings id)
  member x.DumpKit kit = 
    performSysExRequest (DumpKit kit)
  member x.DumpPattern pattern =
    performSysExRequest (DumpPattern pattern)
  member x.QueryStatus statusType =
    performSysExRequest (QueryStatus statusType)
  member x.AssignMachine track machine parameterAssignMode =
    (AssignMachine (track, machine, parameterAssignMode)).Sysex |> outPort.WriteSysex 0
  
  member x.Dump dumpRequest =
    performSysExRequest dumpRequest

  member x.CurrentGlobalSettingsSlot =
    match x.Dump (QueryStatus(MonoMachineStatusType.GlobalSlot)) with
    | Some (MonoMachineSysexResponse.StatusResponse(GlobalSlot, slot)) -> Some slot
    | _ -> None
    
  member x.CurrentGlobalSettings =
      match x.CurrentGlobalSettingsSlot with
      | Some slot -> 
        match x.Dump (MonoMachineSysexRequests.DumpGlobalSettings(slot)) with
        | Some (MonoMachineSysexResponse.GlobalSettings(settings)) -> Some settings
        | _ -> None
      | _ -> None

type VO6Vowel =
| A 
| E 
| EE
| I 
| O 
| U
with
  member x.Params =
    match x with
    | A -> 127uy, 93uy
    | E -> 64uy, 64uy
    | EE -> 93uy, 118uy
    | I -> 91uy, 109uy
    | O -> 99uy, 48uy
    | U -> 71uy, 51uy

type VO6Consonnant =
| NoConsonnant
| B
| D
| F
| G
| H
| J
| K
| L
| M
| N
| P
| R
| RR
| S
| SJ
| T
| TJ
| TH
| V
| Z
with
  member x.Param =
    match x with
    | NoConsonnant -> 3uy
    | B -> 9uy
    | D -> 15uy
    | F -> 21uy
    | G -> 27uy
    | H -> 33uy
    | J -> 39uy
    | K -> 45uy
    | L -> 51uy
    | M -> 57uy
    | N -> 63uy
    | P -> 70uy
    | R -> 76uy
    | RR -> 82uy
    | S -> 88uy
    | SJ -> 94uy
    | T -> 100uy
    | TJ -> 106uy
    | TH -> 112uy
    | V -> 118uy
    | Z -> 124uy

type OneToThree = One | Two | Three

type MonoMachineParameterPage =
| Synthesis
| Amplification
| Filter
| Effects
| LFO of OneToThree

[<RequireQualifiedAccess>]
type LFOTrigMode = Free | Trig | Hold | One | Half



type LFOParameter =
| Page       //of MonoMachineParameterPage
| Dest       //of MonoMachineParameter
| TrigType   //of LFOTrigMode
| Wave       //of LFOShape
| Multiplier //of LFOMultiplier
| Speed      //of speed: byte
| Interlace  //of byte
| Depth      //of byte

and MonoMachineParameter =
| SynthParam1
| SynthParam2
| SynthParam3
| SynthParam4
| SynthParam5
| SynthParam6
| SynthParam7
| SynthParam8
| AmplAttack
| AmplHold
| AmplDecay
| AmplRelease
| AmplDistortion
| AmplVolume
| AmplPan
| AmplPortamento
| FilterBase
| FilterWidth
| FilterHighPassQ
| FilterLowPassQ
| FilterAttack
| FilterDecay
| FilterBaseOffset
| FilterWidthOffset
| EffectEqFrequency
| EffectEqGain
| EffectSampleRateReduction
| EffectDelayTime
| EffectDelaySend
| EffectDelayFeedback
| EffectDelayFilterBase
| EffectDelayFilterWidth
| LFOParameter of OneToThree * LFOParameter

module MonoMachineControlChangeLogic =
    
  let getLFOCCPageBase lfoPage =
    match lfoPage with
    | OneToThree.One -> 88uy
    | OneToThree.Two -> 104uy
    | OneToThree.Three-> 112uy

  let isCCLFOPageForPage page cc =
    let baseCC = getLFOCCPageBase page
    cc >= baseCC && cc < baseCC + 8uy

  let getLfoParameter cc =
    let page = OneToThree.One
    let pageBase = getLFOCCPageBase page
    if isCCLFOPageForPage page cc then
        
        match cc - pageBase with
        | 0uy -> (page, Page      ) |> LFOParameter |> Some
        | 1uy -> (page, Dest      ) |> LFOParameter |> Some
        | 2uy -> (page, TrigType  ) |> LFOParameter |> Some
        | 3uy -> (page, Wave      ) |> LFOParameter |> Some
        | 4uy -> (page, Multiplier) |> LFOParameter |> Some
        | 5uy -> (page, Speed     ) |> LFOParameter |> Some
        | 6uy -> (page, Interlace ) |> LFOParameter |> Some
        | 7uy -> (page, Depth     ) |> LFOParameter |> Some
        | _ -> None
    else
      None

  let getCC parameter =
    match parameter with
    | SynthParam1               -> 48uy | SynthParam2               -> 49uy | SynthParam3               -> 50uy | SynthParam4               -> 51uy
    | SynthParam5               -> 52uy | SynthParam6               -> 53uy | SynthParam7               -> 54uy | SynthParam8               -> 55uy
    | AmplAttack                -> 56uy | AmplHold                  -> 57uy | AmplDecay                 -> 58uy | AmplRelease               -> 59uy
    | AmplDistortion            -> 60uy | AmplVolume                -> 61uy | AmplPan                   -> 62uy | AmplPortamento            -> 63uy
    | FilterBase                -> 72uy | FilterWidth               -> 73uy | FilterHighPassQ           -> 74uy | FilterLowPassQ            -> 75uy
    | FilterAttack              -> 76uy | FilterDecay               -> 77uy | FilterBaseOffset          -> 78uy | FilterWidthOffset         -> 79uy
    | EffectEqFrequency         -> 80uy | EffectEqGain              -> 81uy | EffectSampleRateReduction -> 82uy | EffectDelayTime           -> 83uy
    | EffectDelaySend           -> 84uy | EffectDelayFeedback       -> 85uy | EffectDelayFilterBase     -> 86uy | EffectDelayFilterWidth    -> 87uy
    | LFOParameter(page, Page      ) -> getLFOCCPageBase page + 0uy
    | LFOParameter(page, Dest      ) -> getLFOCCPageBase page + 1uy
    | LFOParameter(page, TrigType  ) -> getLFOCCPageBase page + 2uy
    | LFOParameter(page, Wave      ) -> getLFOCCPageBase page + 3uy
    | LFOParameter(page, Multiplier) -> getLFOCCPageBase page + 4uy
    | LFOParameter(page, Speed     ) -> getLFOCCPageBase page + 5uy
    | LFOParameter(page, Interlace ) -> getLFOCCPageBase page + 6uy
    | LFOParameter(page, Depth     ) -> getLFOCCPageBase page + 7uy

  let getFromCC cc =
    match cc with
    | 48uy -> Some SynthParam1              
    | 49uy -> Some SynthParam2              
    | 50uy -> Some SynthParam3              
    | 51uy -> Some SynthParam4              
    | 52uy -> Some SynthParam5              
    | 53uy -> Some SynthParam6              
    | 54uy -> Some SynthParam7              
    | 55uy -> Some SynthParam8              
    | 56uy -> Some AmplAttack               
    | 57uy -> Some AmplHold                 
    | 58uy -> Some AmplDecay                
    | 59uy -> Some AmplRelease              
    | 60uy -> Some AmplDistortion           
    | 61uy -> Some AmplVolume               
    | 62uy -> Some AmplPan                  
    | 63uy -> Some AmplPortamento           
    | 72uy -> Some FilterBase               
    | 73uy -> Some FilterWidth              
    | 74uy -> Some FilterHighPassQ          
    | 75uy -> Some FilterLowPassQ           
    | 76uy -> Some FilterAttack             
    | 77uy -> Some FilterDecay              
    | 78uy -> Some FilterBaseOffset         
    | 79uy -> Some FilterWidthOffset        
    | 80uy -> Some EffectEqFrequency        
    | 81uy -> Some EffectEqGain             
    | 82uy -> Some EffectSampleRateReduction
    | 83uy -> Some EffectDelayTime          
    | 84uy -> Some EffectDelaySend          
    | 85uy -> Some EffectDelayFeedback      
    | 86uy -> Some EffectDelayFilterBase    
    | 87uy -> Some EffectDelayFilterWidth
    | cc   -> getLfoParameter cc


    
type LFOEvent =
| AssignDest of pickedPage: MonoMachineParameterPage option * pickedDest: byte option * MonoMachineParameter option
| PickShape of LFOShape
| ChangeDepth of depth: byte
| ChangeTime of time: byte * LFOMultiplier
type MonoMachineControlChange =
  | JoystickUp
  | JoystickDown
  | Mute
  | MonoMachineParameter of MonoMachineParameter
  | AllNotesOff
type MonoMachineEvent =
| TrackLevel of Track * value: byte
| TrackParameter of Track * MonoMachineParameter * value: byte
| TrackTrigger of Track * note: byte * velocity: byte
| TrackRelease of Track * note: byte * velocity: byte
| PatternChanged of PatternLocator
| LFOSetting of Track * LFOParameter * byte
| LFOEvent of Track * OneToThree * LFOEvent 
| Unknown of MidiMessage
| MonoMachineSysex of MonoMachineSysexResponse
| Sysex of byte array
| KitChanged of byte
| SequencerStarted
| SequencerStopped
| PatternSelected of PatternLocator
//| ControlChange of MonoMachineControlChange
//| Note of Track * noteNumber: byte * velocity: byte

type TimestampedMessage<'t> = { 
  Timestamp : int
  Message: 't
}

open Midi.MessageMatching
type MonoMachineEventListener(getNow: unit -> int, mm: MonoMachine) =
  let settings = mm.CurrentGlobalSettings
  //let settings = { GlobalSettings.midiBaseChannel = 0uy }
  let midiIn = mm.MidiOutPort
  let midiRealtimeState = Midi.Registers.MidiRealtimeState()
  let event = new Event<_>()

  let midiChannelStates = 
    let tracks =
      [| 
        Track1
        Track2
        Track3
        Track4
        Track5
        Track6
      |]

    tracks
    |> Array.map (fun t -> t, Midi.Registers.MidiChannelState<_>())
    |> dict

  let makeMessage timestamp m = { Timestamp = timestamp; Message = m}
  
  let onChannelMessage (midiEvent: MidiEvent<_>) =
    match settings with
    | None -> Unknown midiEvent.Message
    | Some settings ->
      let message = midiEvent.Message
      let messageChannel = message.Channel.Value
      let midiChannelIsTrack = messageChannel >= settings.midiChannel && messageChannel < (settings.midiChannel + 6uy)
      if midiChannelIsTrack then
        let track = Track.FromByte (byte (messageChannel - settings.midiChannel))
        match message with
        | NoteOn (_, note, velocity)  -> TrackTrigger(track, note, velocity)
        | NoteOff (_, note, velocity) -> TrackRelease(track, note, velocity)
        | ProgramChange {program = program} ->
          let locator = PatternLocator.FromByte program
          PatternSelected(locator)
        //| CC {control} ->
          
          //Unknown message
        | _ ->
          Unknown message

        (*| MessageMatching.CC {Midi.MessageMatching.ControlChange.channel = channel; control; value} ->
          match control with
          | 0x01uy -> JoystickUp   |> MonoMachineEvent.ControlChange
          | 0x02uy -> JoystickDown |> MonoMachineEvent.ControlChange
          | 0x03uy -> Mute         |> MonoMachineEvent.ControlChange
          | _ -> 
            match getFromCC cc with
            | Some mmParam ->
              let lfoEvent =
                match mmParam with
                | LFOParameter(page, LFOParameter.Page) -> AssignDest(Some paramPage, None, None)  |> Some
                | LFOParameter(page, LFOParameter.Dest) -> AssignDest(None, Some value, None)      |> Some
                | LFOParameter(page, LFOParameter.Wave) -> LFOShape.FromCCValue value |> PickShape |> Some
                | _ -> None
              match lfoEvent with
              | Some lfoEvent ->
                  LFOEvent(track, page, lfoEvent)
              | _ -> Unknown message
            | _ ->  Unknown message
        | _ -> Unknown message*)
      else
        Unknown message

  let onSysexMessage sysex =
    Sysex sysex

  let realtimeListener = midiIn.RealtimeMessageReceived.Subscribe(fun m ->
    let oldStarted = midiRealtimeState.started 
    midiRealtimeState.UpdateWithEvent m.Message
    if oldStarted <> midiRealtimeState.started then
      { Timestamp = m.Timestamp; Message = (if midiRealtimeState.started then SequencerStarted else SequencerStopped)} |> event.Trigger
  )
  let channelMessageListener = midiIn.ChannelMessageReceived.Subscribe(fun m ->
    //midiBaseChannel
    let message = onChannelMessage m
    { Timestamp = m.Timestamp; Message = message } |> event.Trigger
  )
  let sysexListener = midiIn.SysexReceived.Subscribe(fun m -> 
    let timestamp = getNow()
    let message = onSysexMessage m
    { Timestamp = timestamp; Message = message } |> event.Trigger
  )
  interface System.IDisposable with
    member x.Dispose() = 
      realtimeListener.Dispose()
      sysexListener.Dispose()
      channelMessageListener.Dispose()
  
  [<CLIEvent>] member x.Event = event.Publish


(*
type MonoMachineEventListener(mnm: MonoMachine, getNow : unit -> int) =
    let mutable mnmGlobals = mnm.CurrentGlobalSettings
    let midiIn = mnm.MidiOutPort
    let event = new Event<_>()

    let onChannelMessage (midiEvent: MidiEvent<_>) =
        let message = midiEvent.Message
        let messageChannel = midiEvent.Message.Channel.Value
        match mnmGlobals with
        | None ->
          Unknown message
        | Some globals ->
        
            let midiBaseChannel = globals.MidiBaseChannel
            let track =
                if messageChannel >= midiBaseChannel && messageChannel < midiBaseChannel + 6 then
                  Some (Track.FromByte (channel - midiBaseChannel))
                else
                  None
            
            match message with
            | ProgramChange {_;program;other} -> PatternChanged (PatternLocator.FromByte program)
            | CC {_;control;value} ->
                match track with
                | Some track ->
                    match MonoMachineControlChangeLogic.getFromCC control with
                    | Some mnmParam -> TrackParameter(track,mnmParam,value)
                    | None          -> Unknown message
                | None -> Unknown message
            | None ->  Unknown message

    let channelMessageListener = midiIn.ChannelMessageReceived.Subscribe(fun m ->
        let message = onChannelMessage m
        { Timestamp = (m.Timestamp); Message = message } |> event.Trigger
    )    
    let onSysexMessage (sysex: byte array) =
        Sysex sysex

    let sysexListener = midiIn.SysexReceived.Subscribe(fun m -> 
        // TODO TODO
        let timestamp = getNow()
        let message = onSysexMessage m
        { Timestamp = timestamp; Message = message } |> event.Trigger
    )
    interface IDisposable with
        member x.Dispose() = 
          sysexListener.Dispose()
          channelMessageListener.Dispose()
          *)