namespace Elektron.MachineDrum
open Elektron
open Elektron.Platform
open Elektron.Platform.SilverMachines
open System.Text
open System.Threading
open System
open System.Diagnostics
open Midi
open Midinette.Platform
open Midinette.Sysex

type NonZeroIndexedArraySegment<'t>(baseLogicalAddress: int, segment: ArraySegment<'t>) =
  let checkAddress address =
    if address < baseLogicalAddress then failwithf "can't query bellow base logical address"
    let address = address - baseLogicalAddress
    if address >= segment.Count then failwithf "can't query after the segment"
    address
  let checkAddressAndLength address length =
    let address = checkAddress address
    if length > segment.Count then failwithf "can't query length past segment"
    address
  member x.LogicalOffset = baseLogicalAddress
  member x.Get address =
   let address = checkAddress address
   segment.Array.[segment.Offset + address]
  member x.Length = segment.Count
  member x.Set address value =
    let address = checkAddress address
    segment.Array.[segment.Offset + address] <- value

  member x.Array = segment.Array.[segment.Offset .. (segment.Count - 1)]
  member x.GetSlice address length =
    let address = checkAddressAndLength address length
    let slice = segment.Array.[segment.Offset + address .. (segment.Offset + address + length - 1)]
    
    assert (slice.Length = length)
    slice
    
  member x.Blit address length data =
    let address = checkAddressAndLength address length
    Array.blit data 0 segment.Array address length
  member x.Segment = segment
  member x.SubSegment baseAddress length =
    let address = checkAddressAndLength baseAddress length
    NonZeroIndexedArraySegment(baseAddress, System.ArraySegment(segment.Array, address, length))

(*
[<RequireQualifiedAccess>]
type LFOType =
| Free
| Trig
| Hold
with
  static member FromByte =
    function
    | 0uy -> Free
    | 1uy -> Trig
    | 2uy -> Hold
    | i -> failwithf "invalid lfo type %i" i
  static member ToByte =
    function
    | Free -> 0uy
    | Trig -> 1uy
    | Hold -> 2uy
*)

type MachineDrumSysexMessageId =
  | Global  = 0x50uy
  | Kit     = 0x52uy
  | Pattern = 0x67uy
  | Song    = 0x69uy
  
module Sysex =
  let mdHeader =
    [|
      0xf0uy
      0x00uy
      0x20uy
      0x3cuy
      0x02uy
      0x00uy
    |] 
    |> UMX.tag_sysex_data
  
  
  
  module Sizes =
    let [<Literal>] kit = 0x4D1
    let [<Literal>] patternShort = 0xacd
    let [<Literal>] patternLong = 0x1522
    
  module Offsets =
    let [<Literal>] messageId = 0x6 
    module Kit =
      let [<Literal>] kit             = 0x9
      let [<Literal>] trackParameters = 0x1a
      let [<Literal>] drumModels      = 0x1aa
      let [<Literal>] reverb          = 0x487
      let [<Literal>] delay           = 0x48f
      let [<Literal>] equalizer       = 0x497
      let [<Literal>] compressor      = 0x49f
    module Pattern =
      let [<Literal>] pattern         = 0x9
    (*  let [<Literal>] trigPattern     = 0xa
      let [<Literal>] lockPattern     = 0x54
      let [<Literal>] accentPattern   = 0x9e
      let [<Literal>] accentAmount    = 0xb1
      let [<Literal>] patternLength   = 0xb2
      let [<Literal>] multiplier      = 0xb3
      *)

  
  let validateSysexShape (sysex: sysex_data) =
    if not (areMachineDrumCheckSumAndLengthValid sysex) then
      failwithf "invalid check sum"
    else
      let messageType = LanguagePrimitives.EnumOfValue (UMX.untag_sysex sysex.[0x6])
      match messageType with
      | MachineDrumSysexMessageId.Kit ->
        if Sizes.kit <> sysex.Length then
          failwithf "kit supposed to be %i but got %i" Sizes.kit sysex.Length
      | MachineDrumSysexMessageId.Pattern ->
        match sysex.Length with
        | Sizes.patternShort | Sizes.patternLong -> ()
        | _ -> failwithf "pattern supposed to be %i or %i but got %i" Sizes.patternShort Sizes.patternLong sysex.Length
      | otherwise ->
          failwithf "non checked message id %A" otherwise

        

[<Struct>]
type LFOType = private SDULFOType of byte
with
  static member Free = SDULFOType 0uy
  static member Trig = SDULFOType 1uy
  static member Hold = SDULFOType 2uy

  static member FromByte b =
    match UMX.untag_byte_7bits b with
    | 0uy -> LFOType.Free
    | 1uy -> LFOType.Trig
    | 2uy -> LFOType.Hold
    | i -> failwithf "invalid lfo type %i" i

  static member ToByte = function | SDULFOType b -> b


module MachineSpecs =
  let patterns       = UMX.tag_byte_7bits [|0uy..127uy|]
  let kits           = UMX.tag_byte_7bits [|0uy..63uy|]
  let songs          = UMX.tag_byte_7bits [|0uy..31uy|]
  let globalSettings = UMX.tag_byte_7bits [|0uy..7uy|]

[<RequireQualifiedAccess>]
type Track =
| BD | SD | HT | MT
| LT | CP | RS | CB
| CH | OH | RC | CC
| M1 | M2 | M3 | M4
with
  static member trackValue track =
    match track with
    | BD -> 0x0uy | SD -> 0x1uy | HT -> 0x2uy | MT -> 0x3uy
    | LT -> 0x4uy | CP -> 0x5uy | RS -> 0x6uy | CB -> 0x7uy 
    | CH -> 0x8uy | OH -> 0x9uy | RC -> 0xauy | CC -> 0xbuy 
    | M1 -> 0xcuy | M2 -> 0xduy | M3 -> 0xeuy | M4 -> 0xfuy
    |> UMX.tag_byte_7bits

  static member trackForValue (value: byte_7bits) =
    match UMX.untag_byte_7bits value with
    | 0x0uy -> BD | 0x1uy -> SD | 0x2uy -> HT | 0x3uy -> MT
    | 0x4uy -> LT | 0x5uy -> CP | 0x6uy -> RS | 0x7uy -> CB 
    | 0x8uy -> CH | 0x9uy -> OH | 0xauy -> RC | 0xbuy -> CC 
    | 0xcuy -> M1 | 0xduy -> M2 | 0xeuy -> M3 | 0xfuy -> M4
    | v -> failwithf "channel %i" v
  static member midiBaseChannelOffset track =
    match track with
    | BD | SD | HT | MT -> 0uy
    | LT | CP | RS | CB -> 1uy
    | CH | OH | RC | CC -> 2uy
    | M1 | M2 | M3 | M4 -> 3uy
    |> UMX.tag_byte_7bits
  static member baseCCParameter track =
    match track with
    | BD | LT | CH | M1 -> 16uy
    | SD | CP | OH | M2 -> 40uy
    | HT | RS | RC | M3 -> 72uy
    | MT | CB | CC | M4 -> 96uy
    |> UMX.tag_byte_7bits
  static member allTracks =
    [|
      BD ; SD ; HT ; MT
      LT ; CP ; RS ; CB
      CH ; OH ; RC ; CC
      M1 ; M2 ; M3 ; M4
    |]

[<RequireQualifiedAccess>]
type MasterEffect =
| Delay
| Reverb
| Equalizer
| Compressor
with
  static member asByte = function | Delay -> 0x5duy | Reverb -> 0x5euy | Equalizer -> 0x5fuy | Compressor -> 0x60uy
  
type MDMachine =
| GND_EM =  00uy | GND_SN =  01uy | GND_NS =  02uy | GND_IM =  03uy
| TRX_BD =  16uy | TRX_SD =  17uy | TRX_XT =  18uy | TRX_CP =  19uy | TRX_RS =  20uy | TRX_CB =  21uy | TRX_CH =  22uy | TRX_OH =  23uy
| TRX_CY =  24uy | TRX_MA =  25uy | TRX_CL =  26uy | TRX_XC =  27uy | TRX_B2 =  28uy
| EFM_BD =  32uy | EFM_SD =  33uy | EFM_XT =  34uy | EFM_CP =  35uy | EFM_RS =  36uy | EFM_CB =  37uy | EFM_HH =  38uy | EFM_CY =  39uy
| E12_BD =  48uy | E12_SD =  49uy | E12_HT =  50uy | E12_LT =  51uy | E12_CP =  52uy | E12_RS =  53uy | E12_CB =  54uy | E12_CH =  55uy
| E12_OH =  56uy | E12_RC =  57uy | E12_CC =  58uy | E12_BR =  59uy | E12_TA =  60uy | E12_TR =  61uy | E12_SH =  62uy | E12_BC =  63uy 
| P_I_BD =  64uy | P_I_SD =  65uy | P_I_MT =  66uy | P_I_ML =  67uy | P_I_MA =  68uy | P_I_RS =  69uy | P_I_RC =  70uy | P_I_CC =  71uy
| P_I_HH =  72uy
| INP_GA =  80uy | INP_GB =  81uy | INP_FA =  82uy | INP_FB =  83uy | INP_EA =  84uy | INP_EB =  85uy
| MID_01 =  96uy | MID_02 =  97uy | MID_03 =  98uy | MID_04 =  99uy | MID_05 = 100uy | MID_06 = 101uy | MID_07 = 102uy | MID_08 = 103uy
| MID_09 = 104uy | MID_10 = 105uy | MID_11 = 106uy | MID_12 = 107uy | MID_13 = 108uy | MID_14 = 109uy | MID_15 = 110uy | MID_16 = 111uy
| CTR_AL = 112uy | CTR_8P = 113uy | CTR_RE = 120uy | CTR_GB = 121uy | CTR_EQ = 122uy | CTR_DX = 123uy

type MDUWMachine =
| ROM_01 = 00uy | ROM_17 = 16uy | ROM_33 = 48uy 
| ROM_02 = 01uy | ROM_18 = 17uy | ROM_34 = 49uy 
| ROM_03 = 02uy | ROM_19 = 18uy | ROM_35 = 50uy 
| ROM_04 = 03uy | ROM_20 = 19uy | ROM_36 = 51uy 
| ROM_05 = 04uy | ROM_21 = 20uy | ROM_37 = 52uy 
| ROM_06 = 05uy | ROM_22 = 21uy | ROM_38 = 53uy 
| ROM_07 = 06uy | ROM_23 = 22uy | ROM_39 = 54uy 
| ROM_08 = 07uy | ROM_24 = 23uy | ROM_40 = 55uy 
| ROM_09 = 08uy | ROM_25 = 24uy | ROM_41 = 56uy 
| ROM_10 = 09uy | ROM_26 = 25uy | ROM_42 = 57uy 
| ROM_11 = 10uy | ROM_27 = 26uy | ROM_43 = 58uy 
| ROM_12 = 11uy | ROM_28 = 27uy | ROM_44 = 59uy 
| ROM_13 = 12uy | ROM_29 = 28uy | ROM_45 = 60uy 
| ROM_14 = 13uy | ROM_30 = 29uy | ROM_46 = 61uy 
| ROM_15 = 14uy | ROM_31 = 30uy | ROM_47 = 62uy 
| ROM_16 = 15uy | ROM_32 = 31uy | ROM_48 = 63uy 
| RAM_R1 = 32uy
| RAM_R2 = 33uy
| RAM_P1 = 34uy
| RAM_P2 = 35uy
| RAM_R3 = 37uy
| RAM_R4 = 38uy
| RAM_P3 = 39uy
| RAM_P4 = 40uy

type MDMachineType =
| MD of MDMachine
| MDUW of MDUWMachine
  member x.HasPitch =
    match x with
    | MDUW (MDUWMachine.RAM_R1|MDUWMachine.RAM_R2|MDUWMachine.RAM_R3|MDUWMachine.RAM_R4) -> false
    | MDUW _ -> true
    | MD (MDMachine.TRX_CH| MDMachine.TRX_CP
            | MDMachine.TRX_OH
            | MDMachine.TRX_CY
            | MDMachine.TRX_MA
            | MDMachine.P_I_MA
            | MDMachine.GND_NS
            | MDMachine.GND_IM
            | MDMachine.INP_EA
            | MDMachine.INP_EB
            | MDMachine.INP_FA
            | MDMachine.INP_FB
            | MDMachine.INP_GA
            | MDMachine.INP_GB
            | MDMachine.CTR_8P
            | MDMachine.CTR_AL
            | MDMachine.CTR_DX
            | MDMachine.CTR_EQ
            | MDMachine.CTR_GB
            | MDMachine.CTR_RE
            | MDMachine.CTR_8P
            | MDMachine.CTR_8P
          ) -> false
    | MD _ -> true


type MDTrackParameter =
| MachineParameter1   | MachineParameter2 | MachineParameter3 | MachineParameter4
| MachineParameter5   | MachineParameter6 | MachineParameter7 | MachineParameter8
| AMDepth             | AMRate            | EQFreq            | EQGain
| FilterBaseFrequency | FilterWidth       | FilterQ           | SampleRateReduction
| Distortion          | Volume            | Pan               | DelaySend
| ReverbSend          | LFOSpeed          | LFOAmount         | LFOShapeMix
with
  static member all = [|
    MachineParameter1   ; MachineParameter2 ; MachineParameter3 ; MachineParameter4
    MachineParameter5   ; MachineParameter6 ; MachineParameter7 ; MachineParameter8
    AMDepth             ; AMRate            ; EQFreq            ; EQGain
    FilterBaseFrequency ; FilterWidth       ; FilterQ           ; SampleRateReduction
    Distortion          ; Volume            ; Pan               ; DelaySend
    ReverbSend          ; LFOSpeed          ; LFOAmount         ; LFOShapeMix    
        
  |]
  static member fromCCOffset offset =
    match UMX.untag_byte_7bits offset with
    | 00uy -> MachineParameter1  | 01uy -> MachineParameter2 | 02uy -> MachineParameter3 | 03uy -> MachineParameter4  
    | 04uy -> MachineParameter5  | 05uy -> MachineParameter6 | 06uy -> MachineParameter7 | 07uy -> MachineParameter8  
    | 08uy -> AMDepth            | 09uy -> AMRate            | 10uy -> EQFreq            | 11uy -> EQGain             
    | 12uy -> FilterBaseFrequency| 13uy -> FilterWidth       | 14uy -> FilterQ           | 15uy -> SampleRateReduction
    | 16uy -> Distortion         | 17uy -> Volume            | 18uy -> Pan               | 19uy -> DelaySend          
    | 20uy -> ReverbSend         | 21uy -> LFOSpeed          | 22uy -> LFOAmount         | 23uy -> LFOShapeMix        
    | v -> failwithf "unknown cc offset: %i" v
  static member getCCOffset parameter =
    match parameter with
    | MachineParameter1   -> 00uy | MachineParameter2 -> 01uy | MachineParameter3 -> 02uy | MachineParameter4   -> 03uy
    | MachineParameter5   -> 04uy | MachineParameter6 -> 05uy | MachineParameter7 -> 06uy | MachineParameter8   -> 07uy
    | AMDepth             -> 08uy | AMRate            -> 09uy | EQFreq            -> 10uy | EQGain              -> 11uy
    | FilterBaseFrequency -> 12uy | FilterWidth       -> 13uy | FilterQ           -> 14uy | SampleRateReduction -> 15uy
    | Distortion          -> 16uy | Volume            -> 17uy | Pan               -> 18uy | DelaySend           -> 19uy
    | ReverbSend          -> 20uy | LFOSpeed          -> 21uy | LFOAmount         -> 22uy | LFOShapeMix         -> 23uy
    |> UMX.tag_byte_7bits
  static member GetCCForTrack parameter track =
    let baseCC = Track.baseCCParameter track
    let offset = MDTrackParameter.getCCOffset parameter
    baseCC + offset
  static member GetParameterForCC (cc: byte_7bits) =
    match UMX.untag_byte_7bits cc with
    | 16uy | 40uy | 72uy |  96uy -> MachineParameter1
    | 17uy | 41uy | 73uy |  97uy -> MachineParameter2
    | 18uy | 42uy | 74uy |  98uy -> MachineParameter3
    | 19uy | 43uy | 75uy |  99uy -> MachineParameter4
    | 20uy | 44uy | 76uy | 100uy -> MachineParameter5
    | 21uy | 45uy | 77uy | 101uy -> MachineParameter6
    | 22uy | 46uy | 78uy | 102uy -> MachineParameter7
    | 23uy | 47uy | 79uy | 103uy -> MachineParameter8
    | 24uy | 48uy | 80uy | 104uy -> AMDepth         
    | 25uy | 49uy | 81uy | 105uy -> AMRate
    | 26uy | 50uy | 82uy | 106uy -> EQFreq
    | 27uy | 51uy | 83uy | 107uy -> EQGain
    | 28uy | 52uy | 84uy | 108uy -> FilterBaseFrequency
    | 29uy | 53uy | 85uy | 109uy -> FilterWidth
    | 30uy | 54uy | 86uy | 110uy -> FilterQ
    | 31uy | 55uy | 87uy | 111uy -> SampleRateReduction
    | 32uy | 56uy | 88uy | 112uy -> Distortion
    | 33uy | 57uy | 89uy | 113uy -> Volume
    | 34uy | 58uy | 90uy | 114uy -> Pan
    | 35uy | 59uy | 91uy | 115uy -> DelaySend
    | 36uy | 60uy | 92uy | 116uy -> ReverbSend
    | 37uy | 61uy | 93uy | 117uy -> LFOSpeed
    | 38uy | 62uy | 94uy | 118uy -> LFOAmount
    | 39uy | 63uy | 95uy | 119uy -> LFOShapeMix
    | i   -> failwithf "unknown parameter for cc %i" i
    
    
    
type MDMachineSettings(bytes: NonZeroIndexedArraySegment<byte_7bits>, trackIndex: int, machineType: MDMachineType) =
  let baseAddress = Sysex.Offsets.Kit.trackParameters
  let getAt a = bytes.Get (a + baseAddress) 
  let setAt address = bytes.Set (address + baseAddress)
  member x.SynthesisParameters          = bytes.GetSlice baseAddress 8
  member x.Parameter1                   = getAt 0
  member x.Parameter2                   = getAt 1
  member x.Parameter3                   = getAt 2
  member x.Parameter4                   = getAt 3
  member x.Parameter5                   = getAt 4
  member x.Parameter6                   = getAt 5
  member x.Parameter7                   = getAt 6
  member x.Parameter8                   = getAt 7
  member x.AmplitudeModulationDepth     = getAt 8
  member x.AmplitudeModulationFrequency = getAt 9
  member x.EqualizerFrequency           = getAt 10
  member x.EqualizerQ                   = getAt 11
  member x.FilterBase                   = getAt 12
  member x.FilterWidth                  = getAt 13
  member x.FilterResonnance             = getAt 14
  member x.SampleRateReduction          = getAt 15
  member x.Distortion                   = getAt 16
  member x.Volume                       = getAt 17
  member x.Pan                          = getAt 18
  member x.DelaySend  with get () = getAt 19 and set v = setAt 19 v
  member x.ReverbSend with get () = getAt 20 and set v = setAt 20 v
  member x.LFOSpeed                     = getAt 21
  member x.LFODepth                     = getAt 22
  member x.LFOShapeMix                  = getAt 23
  member x.GetTrackParameters =
    [|
      MachineParameter1   , x.Parameter1
      MachineParameter2   , x.Parameter2
      MachineParameter3   , x.Parameter3
      MachineParameter4   , x.Parameter4
      MachineParameter5   , x.Parameter5
      MachineParameter6   , x.Parameter6
      MachineParameter7   , x.Parameter7
      MachineParameter8   , x.Parameter8
      AMDepth             , x.AmplitudeModulationDepth
      AMRate              , x.AmplitudeModulationFrequency
      EQFreq              , x.EqualizerFrequency
      EQGain              , x.EqualizerQ
      FilterBaseFrequency , x.FilterBase
      FilterWidth         , x.FilterWidth
      FilterQ             , x.FilterResonnance
      SampleRateReduction , x.SampleRateReduction
      Distortion          , x.Distortion
      Volume              , x.Volume
      Pan                 , x.Pan
      DelaySend           , x.DelaySend
      ReverbSend          , x.ReverbSend
      LFOSpeed            , x.LFOSpeed
      LFOAmount           , x.LFODepth
      LFOShapeMix         , x.LFOShapeMix
    |]
type LFOShape =
| Triangle
| Saw
| Square
| LinearDecay
| ExponentialDecay
| Random
with
  static member FromByte b =
    match UMX.untag_byte_7bits b with
    | 0uy -> Triangle
    | 1uy -> Saw
    | 2uy -> Square
    | 3uy -> LinearDecay
    | 4uy -> ExponentialDecay
    | 5uy -> Random
    | i   -> failwithf "unknown lfo shape %i" i
  static member ToByte =
    function
    | Triangle         -> 0uy
    | Saw              -> 1uy
    | Square           -> 2uy
    | LinearDecay      -> 3uy
    | ExponentialDecay -> 4uy
    | Random           -> 5uy

type MDLFOSetting(bytes: bytes) =
  // sppoooky
  member x.DestinationTrack = bytes.[0] |> UMX.to_byte_7bits |> Track.trackForValue
  member x.DestinationParam = bytes.[1] |> UMX.to_byte_7bits 
  member x.Shape1           = bytes.[2] |> UMX.to_byte_7bits |> LFOShape.FromByte
  member x.Shape2           = bytes.[3] |> UMX.to_byte_7bits |> LFOShape.FromByte
  member x.LFOType          = bytes.[4] |> UMX.to_byte_7bits |> LFOType.FromByte
  member x.Rest             = bytes |> SysexBufferEdit.getSlice 5 31 |> UMX.tag_byte_7bits
  member x.TargetsAnotherLFO = UMX.untag_byte_7bits x.DestinationParam >= 21uy

[<RequireQualifiedAccess>]
type DelayParameter =
| Time 
| ModulationDepth
| ModulationFrequency
| Feedback
| FilterFrequency
| FilterWidth
| Mono
| Level
with
  static member FromByte =
    function
    | 0uy -> Time
    | 1uy -> ModulationDepth
    | 2uy -> ModulationFrequency
    | 3uy -> Feedback
    | 4uy -> FilterFrequency
    | 5uy -> FilterWidth
    | 6uy -> Mono
    | 7uy -> Level
    | v -> failwithf "unknown DelayParameter: %i" v
  static member all =
    [|
       Time 
       ModulationDepth
       ModulationFrequency
       Feedback
       FilterFrequency
       FilterWidth
       Mono
       Level
    |]
    
[<RequireQualifiedAccess>]
type ReverbParameter =
| DelayLevel
| PreDelay
| DecayTime
| Damping
| HighPass
| LowPass
| GateTime
| Level
with
  static member all =
    [|
      DelayLevel
      PreDelay
      DecayTime
      Damping
      HighPass
      LowPass
      GateTime
      Level
    |]
  static member FromByte =
    function
    | 0uy -> DelayLevel
    | 1uy -> PreDelay
    | 2uy -> DecayTime
    | 3uy -> Damping
    | 4uy -> HighPass
    | 5uy -> LowPass
    | 6uy -> GateTime
    | 7uy -> Level
    | i   -> failwithf "unknown reverb parameter %i" i
  static member ToByte =
    function
    | DelayLevel -> 0uy 
    | PreDelay   -> 1uy
    | DecayTime  -> 2uy
    | Damping    -> 3uy
    | HighPass   -> 4uy
    | LowPass    -> 5uy
    | GateTime   -> 6uy
    | Level      -> 7uy
    >> UMX.tag_byte_7bits
    
[<RequireQualifiedAccess>]
type EqualizerParameter =
| LowShelfFrequency
| LowShelfGain
| HighShelfFrequency
| HighShelfGain
| ParametricFrequency
| ParametericGain
| ParametricQ
| Gain
with
  static member FromByte =
    function
    | 0uy -> LowShelfFrequency
    | 1uy -> LowShelfGain
    | 2uy -> HighShelfFrequency
    | 3uy -> HighShelfGain
    | 4uy -> ParametricFrequency
    | 5uy -> ParametericGain
    | 6uy -> ParametricQ
    | 7uy -> Gain           
    | v -> failwithf "unknown EqualizerParameter: %i" v

[<RequireQualifiedAccess>]
type CompressorParameter =
| Attack
| Release
| Threshold
| Ratio
| Knee
| SideChainHighPass
| OutputGain
| Mix
with
  static member FromByte =
    function
    | 0uy -> Attack
    | 1uy -> Release
    | 2uy -> Threshold
    | 3uy -> Ratio
    | 4uy -> Knee
    | 5uy -> SideChainHighPass
    | 6uy -> OutputGain
    | 7uy -> Mix
    | v -> failwithf "unknown CompressorParameter: %i" v
  static member all =
    [|
      Attack
      Release
      Threshold
      Ratio
      Knee
      SideChainHighPass
      OutputGain
      Mix
    |]
    
type EqualizerSettings(bytes: NonZeroIndexedArraySegment<byte_7bits>) =
  let baseAddress = Sysex.Offsets.Kit.equalizer
  let getAt a = bytes.Get (a + baseAddress) 
  let setAt address = bytes.Set (address + baseAddress)
  member x.LowShelfFrequency   with get () = getAt 0 and set v = setAt 0 v
  member x.LowShelfGain        with get () = getAt 1 and set v = setAt 1 v
  member x.HighShelfFrequency  with get () = getAt 2 and set v = setAt 2 v
  member x.HighShelfGain       with get () = getAt 3 and set v = setAt 3 v
  member x.ParametricFrequency with get () = getAt 4 and set v = setAt 4 v
  member x.ParametericGain     with get () = getAt 5 and set v = setAt 5 v
  member x.ParametricQ         with get () = getAt 6 and set v = setAt 6 v
  member x.Gain                with get () = getAt 7 and set v = setAt 7 v

type CompressorSettings(bytes: NonZeroIndexedArraySegment<byte_7bits>) =
  let baseAddress = Sysex.Offsets.Kit.compressor
  let getAt a = bytes.Get (a + baseAddress) 
  let setAt address = bytes.Set (address + baseAddress)
  member x.Attack            with get () = getAt 0 and set v = setAt 0 v
  member x.Release           with get () = getAt 1 and set v = setAt 1 v
  member x.Threshold         with get () = getAt 2 and set v = setAt 2 v
  member x.Ratio             with get () = getAt 3 and set v = setAt 3 v
  member x.Knee              with get () = getAt 4 and set v = setAt 4 v
  member x.SideChainHighPass with get () = getAt 5 and set v = setAt 5 v
  member x.OutputGain        with get () = getAt 6 and set v = setAt 6 v
  member x.Mix               with get () = getAt 7 and set v = setAt 7 v

type DelaySettings(bytes: NonZeroIndexedArraySegment<byte_7bits>) =
  let baseAddress = Sysex.Offsets.Kit.delay
  let getAt a = bytes.Get (a + baseAddress) 
  let setAt address = bytes.Set (address + baseAddress)
  member x.Time                with get () = getAt 0 and set v = setAt 0 v
  member x.Modulation          with get () = getAt 1 and set v = setAt 1 v
  member x.ModulationFrequency with get () = getAt 2 and set v = setAt 2 v
  member x.Feedback            with get () = getAt 3 and set v = setAt 3 v
  member x.FilterFrequency     with get () = getAt 4 and set v = setAt 4 v
  member x.FilterWidth         with get () = getAt 5 and set v = setAt 5 v
  member x.Mono                with get () = getAt 6 and set v = setAt 6 v
  member x.Level               with get () = getAt 7 and set v = setAt 7 v

type ReverbSettings(bytes: NonZeroIndexedArraySegment<byte_7bits>) =
  let baseAddress = Sysex.Offsets.Kit.reverb
  let getAt a = bytes.Get (a + baseAddress) 
  let setAt address = bytes.Set (address + baseAddress)
  member x.DelayToReverb with get () = getAt 0 and set v = setAt 0 v
  member x.PreDelay      with get () = getAt 1 and set v = setAt 1 v
  member x.Decay         with get () = getAt 2 and set v = setAt 2 v
  member x.Damping       with get () = getAt 3 and set v = setAt 3 v
  member x.HighPass      with get () = getAt 4 and set v = setAt 4 v
  member x.LowPass       with get () = getAt 5 and set v = setAt 5 v
  member x.GateTime      with get () = getAt 6 and set v = setAt 6 v
  member x.Level         with get () = getAt 7 and set v = setAt 7 v
  member x.Enumerate () =
    [|
      ReverbParameter.DelayLevel, x.DelayToReverb
      ReverbParameter.PreDelay, x.PreDelay
      ReverbParameter.DecayTime, x.Decay
      ReverbParameter.Damping, x.Damping
      ReverbParameter.HighPass, x.HighPass
      ReverbParameter.LowPass, x.LowPass
      ReverbParameter.GateTime, x.GateTime
      ReverbParameter.Level, x.Level
    |]
  member x.Update parameter value =
    match parameter with
    | ReverbParameter.DelayLevel -> x.DelayToReverb <- value
    | ReverbParameter.PreDelay   -> x.PreDelay <- value
    | ReverbParameter.DecayTime  -> x.Decay <- value
    | ReverbParameter.Damping    -> x.Damping <- value
    | ReverbParameter.HighPass   -> x.HighPass <- value
    | ReverbParameter.LowPass    -> x.LowPass <- value
    | ReverbParameter.GateTime   -> x.GateTime <- value
    | ReverbParameter.Level      -> x.Level <- value

type TrigGroups(bytes: bytes) =
  member x.GetTrig track =
    let index = int (Track.trackValue track)
    let value = UMX.untag_sysex bytes.[index]
    if value < 16uy then
      Some (Track.trackForValue (UMX.tag_byte_7bits value))
    else
      None
  member x.GetMute track =
    let index = int (Track.trackValue track)
    let value = UMX.untag_sysex bytes.[index + 16]
    if value < 16uy then
      Some (Track.trackForValue (UMX.tag_byte_7bits value))
    else
      None

module Fooppp =
  let encode14bits l =
    [|
      byte ((l &&& (0b1111111 <<< 7) >>> 7))
      byte (l &&& 0b1111111)
    |]
    |> unbox

  let checksum bytes =
      let sum = bytes |> Array.map int |> Array.sum
      encode14bits sum

  let makeMessage messageType version revision data =
    [|
      yield! Sysex.mdHeader
      yield  messageType
      yield  version
      yield  revision
      yield! data
      yield! checksum data
      yield! encode14bits (data.Length + 2 + 2)
    |]
open Midi


type MDKit private (bytes: NonZeroIndexedArraySegment<byte_7bits>) =
  do
    assert (bytes.LogicalOffset = Sysex.Offsets.Kit.kit)
  let setDataSlice address expectedLength newBytes =
    let data = byteToData newBytes
    if data.Length <> expectedLength then failwithf "was expecting length of %i but got %i" expectedLength data.Length
    bytes.Blit address data.Length data
    
  let getDataSlice address length =
    let dataSlice = bytes.GetSlice address length
    dataSlice
    |> dataToByte
    
  member x.Position = bytes.Get Sysex.Offsets.Kit.kit
  member x.Name     
    with get ()         = bytes.GetSlice 0x0a 16 |> unbox |> ASCIIEncoding.Default.GetString
    and set (v: string) = bytes.Blit 0x0a 16 (unbox (ASCIIEncoding.Default.GetBytes v))
  
  member x.ReverbSettings     = ReverbSettings     <| bytes.SubSegment Sysex.Offsets.Kit.reverb 8
  member x.DelaySettings      = DelaySettings      <| bytes.SubSegment Sysex.Offsets.Kit.delay 8
  member x.EqualizerSettings  = EqualizerSettings  <| bytes.SubSegment Sysex.Offsets.Kit.equalizer 8
  member x.CompressorSettings = CompressorSettings <| bytes.SubSegment Sysex.Offsets.Kit.compressor 8

  member x.SelectedDrumModel =
    let address = Sysex.Offsets.Kit.drumModels
    getDataSlice address 74 
    |> Array.chunkBySize 4
    |> Array.map fourBytesToBigEndianInt
    |> Array.mapi (fun index i ->
        let b = i &&& 0b1111111 |> byte
        let isUW = i > 127

        let isUwDefined b =
#if FABLE_COMPILER
          failwithf "%i" b
#else
          Enum.IsDefined(typeof<MDUWMachine>, b)
#endif
        let isMdDefined b =
#if FABLE_COMPILER
          failwithf "%i" b
#else
          Enum.IsDefined(typeof<MDMachine>, b)
#endif
        if isUW && isUwDefined b then
          MDUW (LanguagePrimitives.EnumOfValue(b) : MDUWMachine)
        elif (not isUW) && isMdDefined b then
          MD (LanguagePrimitives.EnumOfValue(b) : MDMachine)
        else
          //failwithf "lower 7 bits: %x integer: %x" b i
          failwithf "selectedmodel %i??? lower 7 bits: %x integer: %x" index b i
          //MD (MDMachine.GND_EM)
    )
  member x.AssignDrumModel track drumModel =
    let index = int (Track.trackValue track)
    let address = Sysex.Offsets.Kit.drumModels
    let newBytes = 
      match drumModel with 
      | MD machine -> [|0uy; 0uy; 0uy; byte machine|]
      | MDUW machine -> [|0uy; 0uy; 0uy; (byte machine ||| 0b10000000uy)|]
    let drumModelSlice = getDataSlice address 74 |> unbox |> dataToByte
    SysexBufferEdit.setSlice (index * 4) 4 drumModelSlice (unbox newBytes)

    setDataSlice address 74 (unbox (drumModelSlice |> byteToData))
  member x.MachineParameters =
    Array.init x.SelectedDrumModel.Length (fun i -> MDMachineSettings(bytes, i, x.SelectedDrumModel.[i]))
  member x.LFOSettings =
    let address = 0x1f4
    getDataSlice address 659
    |> Array.chunkBySize 36
    |> Array.map MDLFOSetting
  member x.TrigGroups =
    let address = 0x04a7
    getDataSlice address 37
    |> TrigGroups
  member x.TrackLevels =
    let address = 0x19a
    getDataSlice address 16
  member private x.ContentAsBytes = bytes.Array
  override x.Equals other =
    match other with
    | :? MDKit as other -> x.ContentAsBytes = other.ContentAsBytes
    | _ -> false
  override x.GetHashCode () = hash bytes.Array
    
  static member fromSysex sysex =
    Sysex.validateSysexShape sysex
    let segment = ArraySegment(UMX.to_byte_7bits sysex, Sysex.Offsets.Kit.kit, sysex.Length - 14)
    let segment = NonZeroIndexedArraySegment(Sysex.Offsets.Kit.kit, segment)
    MDKit segment
    
  static member drumModelsAreAllEmpty (kit: MDKit) = kit.SelectedDrumModel = Array.create 16 (MDMachineType.MD MDMachine.GND_EM)


type MDTempoMultiplier =
| One
| Two
| ThreeOverFour
| ThreeOverTwo

type MDPattern private (bytes: NonZeroIndexedArraySegment<byte_7bits>) =
  let isLongFormat =
    bytes.Length > 0x1521
   
  let extraPatternInfo =
    if isLongFormat then
      bytes.GetSlice 0xac6 2647 |> dataToByte
    else
      [||]
  member x.data = bytes
  member x.NumberOfSteps    = bytes.Get 0xb2
  member x.OriginalPosition = bytes.Get 0x9
  member x.AccentAmount     = bytes.Get 0xb1
  member x.AccentPattern    = dataToByte (bytes.GetSlice 0x9e 19)
  member x.LockPattern      = dataToByte (bytes.GetSlice 0x54 74)
  member x.TrigPattern      = 
    let baseTrigs = dataToByte (bytes.GetSlice 0xa 74)
    if isLongFormat then
      Array.concat [baseTrigs; SysexBufferEdit.getSlice 0x0 0x40 extraPatternInfo]
    else
      baseTrigs
  member x.TempoMultiplier  =
    match UMX.untag_byte_7bits (bytes.Get 0xb3) with
    | 0uy -> One
    | 1uy -> Two
    | 2uy -> ThreeOverFour
    | 3uy -> ThreeOverTwo
    | x -> failwithf "%i" x
  member x.Scale = (UMX.untag_sysex (bytes.Get 0xb4) + 1uy) * 16uy
  member x.Kit = bytes.Get 0xb5
  member x.Locks = bytes.GetSlice 0xb7 2341
  override x.GetHashCode () = hash bytes
  override x.Equals other =
    match other with
    | :? MDPattern as other -> x.data = other.data
    | _ -> false
  
  static member fromSysex (sysex: sysex_data) =
    let segment = ArraySegment(UMX.to_byte_7bits sysex, Sysex.Offsets.Pattern.pattern, sysex.Length - 14)
    let segment = NonZeroIndexedArraySegment(Sysex.Offsets.Pattern.pattern ,segment)
    MDPattern segment
        

type MDSong(bytes: sysex_data) =
  member x.data = bytes

type MachineDrumStatusType =
| GlobalSlot
| KitNumber
| PatternNumber
| SongNumber
| SequencerMode
| LockMode
with
  member x.Id =
    UMX.tag_byte_7bits (
      match x with
      | GlobalSlot    -> 0x01uy
      | KitNumber     -> 0x02uy
      | PatternNumber -> 0x04uy
      | SongNumber    -> 0x08uy
      | SequencerMode -> 0x10uy
      | LockMode      -> 0x20uy
    )
  static member FromByte (b: byte) =
    match b with
    | 0x01uy -> GlobalSlot
    | 0x02uy -> KitNumber
    | 0x04uy -> PatternNumber
    | 0x08uy -> SongNumber
    | 0x10uy -> SequencerMode
    | 0x20uy -> LockMode
    | v -> failwithf "unknown status type %i" v

type TriggerType =
| TriggerChannel of mdTrack: Track
| TriggerPattern of pattern: PatternLocator
| UnknownTrigger of value: byte

type NoteTriggerType = NoteTriggerType of note: byte * TriggerType
(*| TriggerChannel of midiNote: byte * Track
| TriggerPattern of midiNote: byte * PatternBank * patternNumber: byte
| Unknown of midiNote: byte * value: byte*)

type KeyMapStructure(bytes: byte array) =
  let isInRange min max value = value >= min && value <= max
  let bankBaseValue =
    function
    | PatternBank.A -> 0x10uy | PatternBank.B -> 0x20uy | PatternBank.C -> 0x30uy | PatternBank.D -> 0x40uy
    | PatternBank.E -> 0x50uy | PatternBank.F -> 0x60uy | PatternBank.G -> 0x70uy | PatternBank.H -> 0x80uy
  let bankForValue value =
    match (value &&& 0xf0uy) with
    | 0x10uy -> PatternBank.A 
    | 0x20uy -> PatternBank.B 
    | 0x30uy -> PatternBank.C 
    | 0x40uy -> PatternBank.D
    | 0x50uy -> PatternBank.E 
    | 0x60uy -> PatternBank.F 
    | 0x70uy -> PatternBank.G 
    | 0x80uy -> PatternBank.H
    | v -> failwithf "bank %i" v
  let getTriggerType (value: byte) =
    if value < 0x10uy then   TriggerChannel (Track.trackForValue (UMX.tag_byte_7bits value))
    elif value < 0x8fuy then TriggerPattern (PatternLocator.PatternLocator(bankForValue value, UMX.tag_byte_7bits ( value &&& 0xfuy)))
    else                     UnknownTrigger (value)
   
  let triggers =
    bytes
    |> Array.indexed
    |> Array.map(fun (note, value) -> NoteTriggerType (byte note, getTriggerType value))
  
  let triggerPerNoteLookup =
    bytes
    |> Array.indexed
    |> Array.map (fun (note, value) -> byte note,getTriggerType value)
    |> dict

  let notePerTriggerLookup =
    bytes
    |> Array.indexed
    |> Array.map (fun (note, value) -> getTriggerType value, byte note)
    |> dict

  member __.data = bytes

  member __.Triggers = triggers

  member __.GetTriggerNotesForBank bank =
    let baseValue = bankBaseValue bank
    bytes
    |> Array.indexed
    |> Array.filter (snd >> (isInRange baseValue (baseValue + 0xfuy)))
    |> Array.map (fun (midiNote, value) ->
      (byte midiNote) , value - baseValue
    )
    |> Array.sortBy snd

  member __.GetTriggerNoteForChannel mdTrack =
    match notePerTriggerLookup.TryGetValue (TriggerChannel mdTrack) with
    | true, note -> Some (UMX.tag_byte_7bits note)
    | _ -> None

  override __.Equals other =
    match other with
    | :? KeyMapStructure as other -> other.data = __.data
    | _ -> false

  override x.GetHashCode() = hash x.data
  interface System.IComparable with
      member x.CompareTo yobj =
          match yobj with
          | :? KeyMapStructure as y -> compare y.data y.data
          | _ -> invalidArg "yobj" "cannot compare values of different types"

type GlobalSettings = {
  mutable OriginalPosition : byte_7bits
  DrumRoutingTable         : Output array
  KeymapStructure          : KeyMapStructure
  mutable MidiBaseChannel  : byte_7bits
  // ...
  // Mechanical settings
  // 24 * Tempo (bit 7...13)
  // 24 * Tempo (bit 0...6)
  // Extended mode
  // External sync
  // Local On
  // Drum Left
  // Drum Right
  // Gate Left
  // Gate Right
  // Sense Left
  // Sense Right
  // Minimum Level Left
  // Minimum Level Right
  // Maximum Level Left
  // Maximum Level Right
  // Program change
  // Trig mode for keymap
}
with 
  static member ToSysex globals =
    [||]
  static member FromSysex (bytes: sysex_data) =
    let originalPosition = bytes.[0x09]
    let drumRoutingTable = bytes |> SysexBufferEdit.getDataSlice 0x0a 16 |> Array.map Output.FromByte
    let keymapStructure = 
      bytes
      |> SysexBufferEdit.getSlice 0x1a 147
      |> unbox
      |> dataToByte
      |> unbox
      |> KeyMapStructure
    let midiBaseChannel = bytes.[0xad]
    { OriginalPosition = UMX.to_byte_7bits originalPosition
      DrumRoutingTable = drumRoutingTable
      KeymapStructure = keymapStructure
      MidiBaseChannel = UMX.to_byte_7bits midiBaseChannel
    }
     
(*
type MDGlobalSettings(bytes: byte array) =
  
  member x.data = bytes
  member x.OriginalPosition = bytes.[0x09]
  member x.DrumRoutingTable = bytes |> getSlice 0x0a 16 |> Array.map Output.FromByte
  member x.KeymapStructure =
    bytes
    |> getSlice 0x1a 147
    |> dataToByte
    |> KeyMapStructure
  member x.MidiBaseChannel =
    bytes.[0xad]
    |> function | 127uy -> None | v -> Some v
    *)


type MachineDrumSysexResponses =
| GlobalSettingsResponse of GlobalSettings
| KitResponse of MDKit
| PatternResponse of MDPattern
| SongResponse of MDSong
| StatusResponse of MachineDrumStatusType * byte_7bits
//| UnknownSysexResponse of byte array
with
  member x.MessageId =
    match x with
    | GlobalSettingsResponse _ -> 0x50uy
    | KitResponse _ -> 0x52uy
    | PatternResponse _ -> 0x67uy
    | SongResponse _ -> 0x69uy
    | StatusResponse _ -> 0x72uy
  static member BuildResponse (sysex: sysex_data) =
    match UMX.untag_sysex sysex.[6] with
    | 0x50uy -> Some (GlobalSettingsResponse (GlobalSettings.FromSysex sysex)             )
    | 0x52uy -> Some (KitResponse (MDKit.fromSysex sysex)                                 )
    | 0x67uy -> Some (PatternResponse (MDPattern.fromSysex sysex)                         )
    | 0x69uy -> Some (SongResponse (MDSong sysex)                                         )
    | 0x72uy -> Some (StatusResponse (MachineDrumStatusType.FromByte (UMX.untag_sysex sysex.[7]), UMX.to_byte_7bits sysex.[8]))
    | _ ->
        // failwithf "h:%x response not understood" sysex.[6]
        None 
        //Some (UnknownSysexResponse sysex)
        

type AssignMachineMode =
| InitSynthesis
| InitSynthesisAndEffects
| InitSynthesisAndEffectsAndRouting
with
  member x.Value =
    UMX.tag_byte_7bits (
      match x with
      | InitSynthesis                     -> 0x0uy
      | InitSynthesisAndEffects           -> 0x1uy
      | InitSynthesisAndEffectsAndRouting -> 0x2uy
    )
open Midi
type MachineDrumSysexRequests =
| DumpGlobalSettings of globalSettingIndex: byte_7bits
| DumpKit            of kit: byte_7bits
| DumpPattern        of pattern: byte_7bits
| DumpSong           of song: byte_7bits
| QueryStatus        of statusType: MachineDrumStatusType
| LoadPattern        of pattern: byte_7bits
| LoadKit            of kit: byte_7bits
| SaveKit            of kit: byte_7bits
| SetCurrentKitName  of string
| AssignMachine      of track: Track * machine: MDMachineType * mode: AssignMachineMode
| SetReverbParameter of ReverbParameter * value: byte_7bits
with
  member x.MessageId = 
    match x with
    | DumpGlobalSettings _ -> 0x51uy
    | DumpKit _            -> 0x53uy
    | AssignMachine _      -> 0x5buy
    | DumpPattern _        -> 0x68uy
    | DumpSong _           -> 0x6auy
    | SetCurrentKitName _  -> 0x56uy
    | LoadPattern _        -> 0x57uy
    | LoadKit _            -> 0x58uy
    | SaveKit _            -> 0x59uy
    | QueryStatus _        -> 0x70uy
    | SetReverbParameter _ -> 0x5euy
  member x.ExpectResponses =
    match x with
    | SaveKit _ | SetReverbParameter _ | SetCurrentKitName _ -> false
    | _         -> true

  member x.ResponseMessageId = 
    match x with 
    | QueryStatus _ -> 0x72uy
    | _ -> x.MessageId - 1uy
  member x.BuildResponse = MachineDrumSysexResponses.BuildResponse
    
  member x.Sysex =
    let data =
      match x with
      | DumpGlobalSettings id  -> id |> Array.singleton
      | DumpKit kit            -> kit |> Array.singleton
      | DumpPattern pattern    -> pattern |> Array.singleton
      | DumpSong song          -> song |> Array.singleton
      | QueryStatus status     -> status.Id |> Array.singleton
      | LoadPattern pattern    -> pattern |> Array.singleton
      | LoadKit kit            -> kit |> Array.singleton
      | SaveKit kit            -> kit |> Array.singleton
      | SetCurrentKitName name -> 
        name.PadRight(16)
        //|> System.Text.Encoding.ASCII.GetBytes 
        |> (fun s -> failwithf "TODO FABLE %s" s; [|0x0uy|])
        |> Seq.truncate 16 
        |> Seq.toArray
        |> Array.map byte
        |> Array.map UMX.tag_byte_7bits
      | AssignMachine (track, machine, mode) ->
        let machineByte, uwByte =
          match machine with
          | MD m   -> UMX.tag_byte_7bits (byte m), UMX.tag_byte_7bits 0uy
          | MDUW m -> UMX.tag_byte_7bits (byte m), UMX.tag_byte_7bits 1uy
        [|Track.trackValue track;machineByte;uwByte;mode.Value|]
      | SetReverbParameter(parameter, value) -> [|ReverbParameter.ToByte parameter;value|]
    
    SysexHelper.makeMachineDrumSysexMessage (Array.concat ([|x.MessageId |> UMX.to_byte_7bits |> Array.singleton; data|]))

type LFOEvent =
| AssignTrack       of Track
| AssignDestination of MDTrackParameter
| AssignShape1      of LFOShape
| AssignShape2      of LFOShape
| AssignType        of LFOType

type MachineDrumEvent =
| TrackLevel        of Track * value: byte_7bits
| TrackParameter    of Track * MDTrackParameter * value: byte_7bits
| TrackTrigger      of Track * velocity: byte_7bits
| TrackRelease      of Track
| PatternChanged    of PatternLocator
| LFOSetting        of Track * LFOEvent
| DelaySetting      of DelayParameter * value: byte_7bits
| ReverbSetting     of ReverbParameter * value: byte_7bits
| EqualizerSetting  of EqualizerParameter * value: byte_7bits
| CompressorSetting of CompressorParameter * value: byte_7bits
| Unknown           of MidiMessage
| MachineDrumSysex  of MachineDrumSysexResponses
| Sysex             of sysex_data
| KitChanged        of byte_7bits
with
    member x.Track =
        match x with
        | TrackLevel (track, _) 
        | TrackParameter(track,_,_)
        | TrackTrigger(track, _)
        | TrackRelease track
        | LFOSetting(track, _)
            -> Some track
        | _ -> None



[<RequireQualifiedAccess>]
type MidiOutputData =
| Message of MidiMessage
| Sysex of bytes: sysex_data

type MachineDrum<'timestamp>(inPort: IMidiInput<'timestamp>, outPort: IMidiOutput<'timestamp>, getSysexNowTimestamp) =
  let helpGetMDSysex maxMessage (timeout: TimeSpan) (request: MachineDrumSysexRequests) inPort : Async<MachineDrumSysexResponses option> =
  #if FABLE_COMPILER
    failwithf "TODO FABLE"
  #else
    let getSysexAsync =
      
      Midinette.Sysex.Sysex.helpGetSysex
          maxMessage
          timeout 
          (fun sysex -> 
              sysex.[0..5] = Sysex.mdHeader 
              && sysex.[6] = UMX.tag_sysex_data request.ResponseMessageId
          ) 
          (request.BuildResponse >> Option.get) 
          inPort
    async {
      let! sysx = getSysexAsync
      match sysx with
      | Choice1Of2 sysx -> return sysx
      | Choice2Of2 exn -> 
        printfn "oops"
        return None
    }
  #endif

  let performSysExRequest (requestMessage: MachineDrumSysexRequests) =
    if requestMessage.ExpectResponses then
      let timeout = TimeSpan.FromMilliseconds(2000.)
  #if FABLE_COMPILER  
      helpGetMDSysex 5 timeout requestMessage inPort |> Async.RunSynchronously  
  #else
      let task = 
        helpGetMDSysex 5 timeout requestMessage inPort
        |> Async.StartAsTask
      requestMessage.Sysex |> outPort.WriteSysex (getSysexNowTimestamp())
      task.Result
#endif
    else
      None


  member x.MidiOutPort = inPort
  member x.MidiInPort  = outPort
   
  member x.EventToMidiMessages (mdEvent: MachineDrumEvent) globals =
      let channel = globals.MidiBaseChannel
      //globals.KeymapStructure.GetTriggerNotesForBank
      let track = mdEvent.Track
      let note = 
          match track with
          | Some track -> globals.KeymapStructure.GetTriggerNoteForChannel track
          | None -> None

      let makeNote note velocity isOn =
          match note with
          | None      -> None
          | Some note -> 
              let note = note
              let velocity = velocity
              if isOn then
                  Some (MidiOutputData.Message (MidiMessage.NoteOn channel note velocity))
              else
                  Some (MidiOutputData.Message (MidiMessage.NoteOff channel note velocity))
      let makeCC track parameter value =
        let cc      = UMX.untag_byte_7bits <| MDTrackParameter.GetCCForTrack parameter track
        let channel = UMX.untag_byte_7bits <| Track.midiBaseChannelOffset track + channel
        let value   = UMX.untag_byte_7bits <| value
        MidiMessage.CC channel cc value
        |> MidiOutputData.Message
      let makeProgramChange program =
        let program = UMX.untag_byte_7bits program
        MidiMessage.ProgramChange channel program
        |> MidiOutputData.Message
        
      let none = Array.empty
      let some = Array.singleton
      match mdEvent with
      | TrackTrigger(track, velocity)            -> makeNote note velocity true |> Option.get |> some
      | TrackRelease(track)                      -> makeNote note (UMX.tag_byte_7bits 0uy) false |> Option.get |> some
      | TrackLevel(track, level)                 -> none
      | TrackParameter(track, parameter, value)  -> makeCC track parameter value |> some
      | PatternChanged pattern                   -> none
      | Unknown message                          -> message    |> MidiOutputData.Message |> some
      | Sysex data                               -> data       |> MidiOutputData.Sysex   |> some
      | MachineDrumSysex sysex                   -> none
      | KitChanged kit                           -> makeProgramChange kit |> some
      | _ -> none
          

  
  member x.SendEvents getNow mdGlobals mdEvents =
      match mdGlobals with
      | Some globals -> 
          let now = getNow ()
          for timestamp, mdEvent in mdEvents do
              let timestamp = timestamp // + now /// TDODODODODODO??
              x.EventToMidiMessages mdEvent globals 
              |> Array.iter (
                function
                | MidiOutputData.Message message -> outPort.WriteMessage timestamp message
                | MidiOutputData.Sysex sysex     -> outPort.WriteSysex now sysex
              )
      | None -> ()

  member x.QueryStatus statusType = performSysExRequest (QueryStatus statusType)

  member x.Dump dumpRequest = performSysExRequest dumpRequest
  member x.CurrentGlobalSettingsSlot =
    match x.Dump (QueryStatus(MachineDrumStatusType.GlobalSlot)) with
    | Some (MachineDrumSysexResponses.StatusResponse(GlobalSlot, slot)) -> Some slot
    | _ -> None

  member x.CurrentGlobalSettings =
    match x.CurrentGlobalSettingsSlot with
    | Some slot ->
      match x.Dump (MachineDrumSysexRequests.DumpGlobalSettings(slot)) with
      | Some (MachineDrumSysexResponses.GlobalSettingsResponse(settings)) -> Some settings
      | _ -> None
    | None -> None

  member x.CurrentKitIndex =
    match x.Dump (QueryStatus(MachineDrumStatusType.KitNumber)) with
    | Some (MachineDrumSysexResponses.StatusResponse(KitNumber, kit)) -> Some kit
    | _ -> None

  member x.DumpCurrentKit =
    match x.CurrentKitIndex with
    | Some kit ->
      match x.Dump (MachineDrumSysexRequests.DumpKit(kit)) with
      | Some (MachineDrumSysexResponses.KitResponse(kit)) -> Some kit
      | _ -> None
    | _ -> None

  member x.AssignMachine track machine mode =
    (AssignMachine (track, machine, mode)).Sysex
    |> outPort.WriteSysex (getSysexNowTimestamp())

   member x.ChangeTrackParameter globals track parameter value =
    let cc = MDTrackParameter.GetCCForTrack parameter track    
    MidiMessage.CC (UMX.untag_byte_7bits globals.MidiBaseChannel) cc value
    |> outPort.WriteMessage (getSysexNowTimestamp())
(*  
  member x.DumpKit kit =
    performSysExRequest (DumpKit kit)

  member x.DumpGlobalSettings id =
    performSysExRequest (DumpGlobalSettings id)
  member x.DumpPattern pattern =
    performSysExRequest (DumpPattern pattern)*)


module MachineDrumEventParser =
  let inline isMachineDrumChannel (midiBaseChannel: byte_7bits) channel = channel >= midiBaseChannel && channel < midiBaseChannel + (UMX.tag_byte_7bits 4uy)
  let inline isMachineDrumControlChange midiBaseChannel (message: MidiMessage) = 
    message.MessageType = MidiMessageType.ControllerChange 
    && isMachineDrumChannel midiBaseChannel message.Channel.Value
    && (message.Data1 >= 16uy && message.Data1 <= 119uy)
  let trackOffset cc =
    let cc = UMX.untag_byte_7bits cc
    UMX.tag_byte_7bits(
      if   cc <= 39uy  then 0uy
      elif cc <= 63uy  then 1uy
      elif cc <= 95uy  then 2uy
      else                  3uy
    )

type TimestampedMessage<'t,'timestamp> = { 
  Timestamp : 'timestamp
  Message: 't
}

type MachineDrumEventListener<'timestamp>(md: MachineDrum<'timestamp>, getNow) =
  let mutable mdGlobalSettings = md.CurrentGlobalSettings
  let midiIn = md.MidiOutPort
  //let mutable lastKit = {Timestamp = 0; Message = None }
  let event = new Event<_>()
  let onChannelMessage (midiEvent: MidiEvent<_>) =
    let message = midiEvent.Message
    match mdGlobalSettings with
    | None -> 
        Unknown midiEvent.Message
    | Some mdGlobalSettings ->
        let midiBaseChannel = mdGlobalSettings.MidiBaseChannel 
        if message.MessageType = MidiMessageType.ProgramChange && message.Channel = Some midiBaseChannel then
          PatternChanged (PatternLocator.FromByte message.Data1)
        elif MachineDrumEventParser.isMachineDrumControlChange midiBaseChannel message then
          let channel = message.Channel.Value
          let cc = UMX.tag_byte_7bits message.Data1
          let trackOffset = MachineDrumEventParser.trackOffset cc

          let midiChannelOffset = channel - midiBaseChannel
          let track = Track.trackForValue ((midiChannelOffset * 4uy) + trackOffset)
          let parameter = MDTrackParameter.GetParameterForCC cc
          TrackParameter (track, parameter, UMX.tag_byte_7bits message.Data2)
        elif message.MessageType = MidiMessageType.ControllerChange && message.Data1 >= 8uy && message.Data1 <= 0xbuy then
          let midiChannelOffset = message.Channel.Value - midiBaseChannel
      
          let track =
            match message.Data1, UMX.untag_byte_7bits midiChannelOffset with
            | 0x08uy, 0uy -> Some Track.BD
            | 0x09uy, 0uy -> Some Track.SD
            | 0x0auy, 0uy -> Some Track.HT
            | 0x0buy, 0uy -> Some Track.MT
            | 0x08uy, 1uy -> Some Track.LT
            | 0x09uy, 1uy -> Some Track.CP
            | 0x0auy, 1uy -> Some Track.RS
            | 0x0buy, 1uy -> Some Track.CB
            | 0x08uy, 2uy -> Some Track.CH
            | 0x09uy, 2uy -> Some Track.OH
            | 0x0auy, 2uy -> Some Track.RC
            | 0x0buy, 2uy -> Some Track.CC
            | 0x08uy, 3uy -> Some Track.M1
            | 0x09uy, 3uy -> Some Track.M2
            | 0x0auy, 3uy -> Some Track.M3
            | 0x0buy, 3uy -> Some Track.M4
            | _ -> None

          match track with
          | Some track -> TrackLevel(track, UMX.tag_byte_7bits message.Data2)
          | None -> Unknown message

        elif message.MessageType = MidiMessageType.NoteOn && message.Channel.Value = midiBaseChannel then
          let noteNumber = message.Data1
      
          match mdGlobalSettings.KeymapStructure.Triggers.[int noteNumber] with
          | NoteTriggerType.NoteTriggerType(_, TriggerChannel track) ->
            match message.Data2 with
            | 0uy      -> TrackRelease track
            | velocity -> TrackTrigger(track, UMX.tag_byte_7bits velocity)
          | _ -> Unknown message
        else
          Unknown message

  let onSysexMessage (sysex: sysex_data) =
    match UMX.untag_sysex sysex with
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x5duy; paramIndex; paramValue; 0xf7uy|] ->
      DelaySetting(DelayParameter.FromByte paramIndex, UMX.tag_byte_7bits paramValue)
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x5euy; paramIndex; paramValue; 0xf7uy|] ->
      ReverbSetting(ReverbParameter.FromByte paramIndex, UMX.tag_byte_7bits paramValue)
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x5fuy; paramIndex; paramValue; 0xf7uy|] ->
      EqualizerSetting(EqualizerParameter.FromByte paramIndex, UMX.tag_byte_7bits paramValue)
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x60uy; paramIndex; paramValue; 0xf7uy|] ->
      CompressorSetting(CompressorParameter.FromByte paramIndex, UMX.tag_byte_7bits paramValue)
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x62uy; paramIndex; paramValue; 0xf7uy|] ->
      let lfoIndex = (paramIndex &&& 0b01111000uy) >>> 3
      let track = Track.trackForValue (UMX.tag_byte_7bits lfoIndex)
      let lfoSettingIndex = paramIndex &&& 0b00000111uy
      let paramValue = UMX.tag_byte_7bits paramValue
      match lfoSettingIndex with
      | 0x0uy -> LFOSetting(track, AssignTrack       (Track.trackForValue paramValue))
      | 0x1uy -> LFOSetting(track, AssignDestination (MDTrackParameter.fromCCOffset paramValue))
      | 0x2uy -> LFOSetting(track, AssignShape1      (LFOShape.FromByte paramValue))
      | 0x3uy -> LFOSetting(track, AssignShape2      (LFOShape.FromByte paramValue))
      | 0x4uy -> LFOSetting(track, AssignType        (LFOType.FromByte paramValue))
      | 0x5uy -> TrackParameter(track, MDTrackParameter.LFOSpeed, paramValue)
      | 0x6uy -> TrackParameter(track, MDTrackParameter.LFOAmount, paramValue)
      | 0x7uy -> TrackParameter(track, MDTrackParameter.LFOShapeMix, paramValue)
      | _ -> Sysex sysex
    | _ ->
     if sysex.[0..5] = Sysex.mdHeader then
      MachineDrumSysex (MachineDrumSysexResponses.BuildResponse sysex).Value
     else
      Sysex sysex

  let channelMessageListener = midiIn.ChannelMessageReceived.Subscribe(fun m ->
    let message = onChannelMessage m
    { Timestamp = (m.Timestamp); Message = message } |> event.Trigger
  )
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
  
  [<CLIEvent>] member x.Event = event.Publish
