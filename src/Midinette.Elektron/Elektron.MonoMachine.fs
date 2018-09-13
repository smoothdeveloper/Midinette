module Elektron.MonoMachine

open System
open Midi
open Elektron.Platform
open Elektron.Platform.SilverMachines

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
  static member FromSysex (bytes: byte array) =
     let globalData = (getMonoMachineDataSliceFromSysexMessage bytes) |> Seq.toArray |> Elektron.Platform.byteToData
     
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
type MonoMachineSysexResponse =
| GlobalSettings of byte array
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
    | DumpGlobalSettings _ -> GlobalSettings data
    | DumpKit _            -> Kit(MonoMachineKit data)
    | DumpPattern _        -> Pattern data
    | DumpSong _           -> Song data
    | QueryStatus _        -> StatusResponse((MonoMachineStatusType.FromByte data.[monoMachineHeader.Length + 1]), data.[monoMachineHeader.Length + 2])
    | AssignMachine(_)     -> failwithf "not implemented"
    //| _ -> None

type MonoMachineTrig =
| Amp    = 0b0001uy
| Filter = 0b010uy
| LFO    = 0b0100uy


type MonoMachine(inPort: IMidiInput<_>, outPort: IMidiOutput<_>) =
  let helpGetMonomachineSysex maxMessage (timeout: TimeSpan) (request: MonoMachineSysexRequests) (inPort: IMidiInput<_>) =
    Midi.Sysex.helpGetSysex maxMessage timeout (fun sysex -> 
      sysex.[0..5] = Helpers.monoMachineHeader && Some sysex.[6] = request.ResponseMessageId) request.BuildResponse inPort

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
type MonoMachineParameter =
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
| LFOPage
| LFODest
| LFOTrig