module Elektron.MachineDrum
open Elektron
open Elektron.Platform
open Elektron.Platform.SilverMachines
open System.Text
open System.Threading
open System
open System.Diagnostics
open Midi

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


module MachineSpecs =
  let patterns = [|0uy..127uy|]
  let kits     = [|0uy..63uy|]
  let songs    = [|0uy..31uy|]


  
[<RequireQualifiedAccess>]
type Track =
| BD | SD | HT | MT
| LT | CP | RS | CB
| CH | OH | RC | CC
| M1 | M2 | M3 | M4
with
  static member trackValue =
    function
    | BD -> 0x0uy | SD -> 0x1uy | HT -> 0x2uy | MT -> 0x3uy
    | LT -> 0x4uy | CP -> 0x5uy | RS -> 0x6uy | CB -> 0x7uy 
    | CH -> 0x8uy | OH -> 0x9uy | RC -> 0xauy | CC -> 0xbuy 
    | M1 -> 0xcuy | M2 -> 0xduy | M3 -> 0xeuy | M4 -> 0xfuy
  static member trackForValue =
    function
    | 0x0uy -> BD | 0x1uy -> SD | 0x2uy -> HT | 0x3uy -> MT
    | 0x4uy -> LT | 0x5uy -> CP | 0x6uy -> RS | 0x7uy -> CB 
    | 0x8uy -> CH | 0x9uy -> OH | 0xauy -> RC | 0xbuy -> CC 
    | 0xcuy -> M1 | 0xduy -> M2 | 0xeuy -> M3 | 0xfuy -> M4
    | v -> failwithf "channel %i" v
  static member midiBaseChannelOffset =
    function
    | BD | SD | HT | MT -> 0uy
    | LT | CP | RS | CB -> 1uy
    | CH | OH | RC | CC -> 2uy
    | M1 | M2 | M3 | M4 -> 3uy
  static member baseCCParameter =
    function
    | BD | LT | CH | M1 -> 16uy
    | SD | CP | OH | M2 -> 40uy
    | HT | RS | RC | M3 -> 72uy
    | MT | CB | CC | M4 -> 96uy

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
  static member fromCCOffset =
    function
    | 00uy -> MachineParameter1  | 01uy -> MachineParameter2 | 02uy -> MachineParameter3 | 03uy -> MachineParameter4  
    | 04uy -> MachineParameter5  | 05uy -> MachineParameter6 | 06uy -> MachineParameter7 | 07uy -> MachineParameter8  
    | 08uy -> AMDepth            | 09uy -> AMRate            | 10uy -> EQFreq            | 11uy -> EQGain             
    | 12uy -> FilterBaseFrequency| 13uy -> FilterWidth       | 14uy -> FilterQ           | 15uy -> SampleRateReduction
    | 16uy -> Distortion         | 17uy -> Volume            | 18uy -> Pan               | 19uy -> DelaySend          
    | 20uy -> ReverbSend         | 21uy -> LFOSpeed          | 22uy -> LFOAmount         | 23uy -> LFOShapeMix        
    | v -> failwithf "unknown cc offset: %i" v
  static member getCCOffset =
    function
    | MachineParameter1   -> 00uy | MachineParameter2 -> 01uy | MachineParameter3 -> 02uy | MachineParameter4   -> 03uy
    | MachineParameter5   -> 04uy | MachineParameter6 -> 05uy | MachineParameter7 -> 06uy | MachineParameter8   -> 07uy
    | AMDepth             -> 08uy | AMRate            -> 09uy | EQFreq            -> 10uy | EQGain              -> 11uy
    | FilterBaseFrequency -> 12uy | FilterWidth       -> 13uy | FilterQ           -> 14uy | SampleRateReduction -> 15uy
    | Distortion          -> 16uy | Volume            -> 17uy | Pan               -> 18uy | DelaySend           -> 19uy
    | ReverbSend          -> 20uy | LFOSpeed          -> 21uy | LFOAmount         -> 22uy | LFOShapeMix         -> 23uy
  static member GetCCForTrack parameter track =
    let baseCC = Track.baseCCParameter track
    let offset = MDTrackParameter.getCCOffset parameter
    baseCC + offset
  static member GetParameterForCC cc =
    match cc with
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
type MDMachineSettings(bytes: byte array, offset: int, machineType: MDMachineType) =
  let baseAddress = 0x1a + (offset * 24)
  let getAt a = bytes.[baseAddress + a]
  let setAt a v = bytes.[baseAddress + a] <- (v &&& 0b01111111uy)
  member x.SynthesisParameters = getSlice baseAddress 8 bytes
  member x.Parameter1                   = bytes.[baseAddress + 0]
  member x.Parameter2                   = bytes.[baseAddress + 1]
  member x.Parameter3                   = bytes.[baseAddress + 2]
  member x.Parameter4                   = bytes.[baseAddress + 3]
  member x.Parameter5                   = bytes.[baseAddress + 4]
  member x.Parameter6                   = bytes.[baseAddress + 5]
  member x.Parameter7                   = bytes.[baseAddress + 6]
  member x.Parameter8                   = bytes.[baseAddress + 7]
  member x.AmplitudeModulationDepth     = bytes.[baseAddress + 8]
  member x.AmplitudeModulationFrequency = bytes.[baseAddress + 9]
  member x.EqualizerFrequency           = bytes.[baseAddress + 10]
  member x.EqualizerQ                   = bytes.[baseAddress + 11]
  member x.FilterBase                   = bytes.[baseAddress + 12]
  member x.FilterWidth                  = bytes.[baseAddress + 13]
  member x.FilterResonnance             = bytes.[baseAddress + 14]
  member x.SampleRateReduction          = bytes.[baseAddress + 15]
  member x.Distortion                   = bytes.[baseAddress + 16]
  member x.Volume                       = bytes.[baseAddress + 17]
  member x.Pan                          = bytes.[baseAddress + 18]
  member x.DelaySend                    with get () = getAt 19 and set v   = setAt 19 v
  member x.ReverbSend                   with get () = getAt 20 and set v   = setAt 20 v
  member x.LFOSpeed                     = bytes.[baseAddress + 21]
  member x.LFODepth                     = bytes.[baseAddress + 22]
  member x.LFOShapeMix                  = bytes.[baseAddress + 23]
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
  static member FromByte =
    function
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

type MDLFOSetting(bytes: byte array) =
  member x.DestinationTrack = bytes.[0] |> Track.trackForValue
  member x.DestinationParam = bytes.[1]
  member x.Shape1           = bytes.[2] |> LFOShape.FromByte
  member x.Shape2           = bytes.[3] |> LFOShape.FromByte
  member x.LFOType          = bytes.[4] |> LFOType.FromByte
  member x.Rest             = bytes |> getSlice 5 31
  member x.TargetsAnotherLFO = x.DestinationParam >= 21uy

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
  static member All =
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

type EqualizerSettings(bytes: byte array) =
  let baseAddress = 0x497
  let getAt a = bytes.[baseAddress + a]
  let setAt a v = bytes.[baseAddress + a] <- (v &&& 0b01111111uy)
  member x.LowShelfFrequency   with get () = getAt 0 and set v = setAt 0 v
  member x.LowShelfGain        with get () = getAt 1 and set v = setAt 1 v
  member x.HighShelfFrequency  with get () = getAt 2 and set v = setAt 2 v
  member x.HighShelfGain       with get () = getAt 3 and set v = setAt 3 v
  member x.ParametricFrequency with get () = getAt 4 and set v = setAt 4 v
  member x.ParametericGain     with get () = getAt 5 and set v = setAt 5 v
  member x.ParametricQ         with get () = getAt 6 and set v = setAt 6 v
  member x.Gain                with get () = getAt 7 and set v = setAt 7 v

type CompressorSettings(bytes: byte array) =
  let baseAddress = 0x49f
  let getAt a = bytes.[baseAddress + a]
  let setAt a v = bytes.[baseAddress + a] <- (v &&& 0b01111111uy)
  member x.Attack            with get () = getAt 0 and set v = setAt 0 v
  member x.Release           with get () = getAt 1 and set v = setAt 1 v
  member x.Threshold         with get () = getAt 2 and set v = setAt 2 v
  member x.Ratio             with get () = getAt 3 and set v = setAt 3 v
  member x.Knee              with get () = getAt 4 and set v = setAt 4 v
  member x.SideChainHighPass with get () = getAt 5 and set v = setAt 5 v
  member x.OutputGain        with get () = getAt 6 and set v = setAt 6 v
  member x.Mix               with get () = getAt 7 and set v = setAt 7 v












type DelaySettings(bytes: byte array) =
  let baseAddress = 0x48f
  let getAt a = bytes.[baseAddress + a]
  let setAt a v = bytes.[baseAddress + a] <- (v &&& 0b01111111uy)
  member x.Time                with get () = getAt 0 and set v = setAt 0 v
  member x.Modulation          with get () = getAt 1 and set v = setAt 1 v
  member x.ModulationFrequency with get () = getAt 2 and set v = setAt 2 v
  member x.Feedback            with get () = getAt 3 and set v = setAt 3 v
  member x.FilterFrequency     with get () = getAt 4 and set v = setAt 4 v
  member x.FilterWidth         with get () = getAt 5 and set v = setAt 5 v
  member x.Mono                with get () = getAt 6 and set v = setAt 6 v
  member x.Level               with get () = getAt 7 and set v = setAt 7 v

type ReverbSettings(bytes: byte array) =
  let baseAddress = 0x487
  let getAt a = bytes.[baseAddress + a]
  let setAt a v = bytes.[baseAddress + a] <- (v &&& 0b01111111uy)
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

type TrigGroups(bytes: byte array) =
  member x.GetTrig track =
    let index = int (Track.trackValue track)
    let value = bytes.[index]
    if value < 16uy then
      Some (Track.trackForValue value)
    else
      None
  member x.GetMute track =
    let index = int (Track.trackValue track)
    let value = bytes.[index + 16]
    if value < 16uy then
      Some (Track.trackForValue value)
    else
      None

type MDKit(bytes: byte array) =
  let setDataSlice s l = setSlice s l bytes
  let getDataSlice s l = getSlice s l bytes
  member x.data = bytes
  member x.Position = bytes.[0x09]
  
  
  #if FABLE_COMPILER
  #else
  member x.Name     
    with get ()         = getDataSlice 0x0a 16 |> ASCIIEncoding.Default.GetString
    and set (v: string) = setDataSlice 0x0a 16 (ASCIIEncoding.Default.GetBytes v)
  #endif
  member x.ReverbSettings = ReverbSettings bytes
  member x.DelaySettings = DelaySettings bytes
  member x.EqualizerSettings = EqualizerSettings bytes
  member x.CompressorSettings = CompressorSettings bytes
  member x.unpacked =
    if areMachineDrumCheckSumAndLengthValid bytes then
      getMachineDrumDataSliceFromSysexMessage bytes
      |> Seq.toArray
      |> dataToByte
    else [||]
  member x.SelectedDrumModel =
    let address = 0x1aa
    getDataSlice address 74 
    |> dataToByte
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
    
    //let models = x.SelectedDrumModel
    let index = int (Track.trackValue track)
    //models.[index] <- drumModel
    
    let address = 0x1aa
    let newBytes = 
      match drumModel with 
      | MD machine -> [|0uy; 0uy; 0uy; byte machine|]
      | MDUW machine -> [|0uy; 0uy; 0uy; (byte machine ||| 0b10000000uy)|]
    
    let drumModelSlice = getDataSlice address 74 |> dataToByte
    setSlice (index * 4) 4 drumModelSlice newBytes

    setDataSlice address 74 (drumModelSlice |> byteToData)


  member x.MachineParameters =
    Array.init x.SelectedDrumModel.Length (fun i -> MDMachineSettings(bytes, i, x.SelectedDrumModel.[i]))
  member x.LFOSettings =
    let address = 0x1f4
    getDataSlice address 659
    |> dataToByte
    |> Array.chunkBySize 36
    |> Array.map MDLFOSetting
  member x.TrigGroups =
    let address = 0x04a7
    getDataSlice address 37
    |> dataToByte
    |> TrigGroups
  member x.TrackLevels =
    let address = 0x19a
    getDataSlice address 16
  static member SelectedDrumModelAllEmpty (kit: MDKit) =
    kit.SelectedDrumModel = Array.create 16 (MDMachineType.MD MDMachine.GND_EM)


type MDTempoMultiplier =
| One
| Two
| ThreeOverFour
| ThreeOverTwo

let inline getSlice offset length (array : _ array) =
  array.[offset .. (offset + length - 1)]

type MDPattern(bytes: byte array) =
  let isLongFormat =
    bytes.Length > 0x1521
  let getBytes offset length =
    bytes 
    |> getSlice offset length
    |> dataToByte
    
  //let getSlice offset length array =
  let extraPatternInfo =
    if isLongFormat then
      getBytes 0xac6 2647
    else
      [||]
  member x.data = bytes
  member x.NumberOfSteps    = bytes.[0xb2]
  member x.OriginalPosition = bytes.[0x9]
  member x.AccentAmount     = bytes.[0xb1]
  member x.AccentPattern    = getBytes 0x9e 19
  member x.LockPattern      = getBytes 0x54 74
  member x.TrigPattern      = 
    let baseTrigs = getBytes 0xa 74
    if isLongFormat then
      Array.concat [baseTrigs; getSlice 0x0 0x40 extraPatternInfo]
    else
      baseTrigs
  member x.TempoMultiplier  =
    match bytes.[0xb3] with
    | 0uy -> One
    | 1uy -> Two
    | 2uy -> ThreeOverFour
    | 3uy -> ThreeOverTwo
    | x -> failwithf "%i" x
  member x.Scale = (bytes.[0xb4] + 1uy) * 16uy
  member x.Kit = bytes.[0xb5]
  member x.Locks = getBytes 0xb7 2341

type MDSong(bytes: byte array) =
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
    match x with
    | GlobalSlot    -> 0x01uy
    | KitNumber     -> 0x02uy
    | PatternNumber -> 0x04uy
    | SongNumber    -> 0x08uy
    | SequencerMode -> 0x10uy
    | LockMode      -> 0x20uy
  static member FromByte =
    function
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
    if value < 0x10uy then   TriggerChannel (Track.trackForValue value)
    elif value < 0x8fuy then TriggerPattern (PatternLocator.PatternLocator(bankForValue value, value &&& 0xfuy))
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
    | true, note -> Some note
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
  mutable OriginalPosition : byte
  DrumRoutingTable         : Output array
  KeymapStructure          : KeyMapStructure
  mutable MidiBaseChannel  : byte
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
  static member FromSysex (bytes: byte array) =
    let originalPosition = bytes.[0x09]
    let drumRoutingTable = bytes |> getSlice 0x0a 16 |> Array.map Output.FromByte
    let keymapStructure = 
      bytes
      |> getSlice 0x1a 147
      |> dataToByte
      |> KeyMapStructure
    let midiBaseChannel = bytes.[0xad]
    { OriginalPosition = originalPosition
      DrumRoutingTable = drumRoutingTable
      KeymapStructure = keymapStructure
      MidiBaseChannel = midiBaseChannel
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
type MachineDrumSysexMessageId =
| Kit     = 0x52uy
| Pattern = 0x67uy
| Song    = 0x69uy

type MachineDrumSysexResponses =
| GlobalSettingsResponse of GlobalSettings
| KitResponse of MDKit
| PatternResponse of MDPattern
| SongResponse of MDSong
| StatusResponse of MachineDrumStatusType * byte
//| UnknownSysexResponse of byte array
with
  member x.MessageId =
    match x with
    | GlobalSettingsResponse _ -> 0x50uy
    | KitResponse _ -> 0x52uy
    | PatternResponse _ -> 0x67uy
    | SongResponse _ -> 0x69uy
    | StatusResponse _ -> 0x72uy
  static member BuildResponse (sysex: byte array) =
    match sysex.[6] with
    | 0x50uy -> Some (GlobalSettingsResponse (GlobalSettings.FromSysex sysex)             )
    | 0x52uy -> Some (KitResponse (MDKit sysex)                                           )
    | 0x67uy -> Some (PatternResponse (MDPattern sysex)                                   )
    | 0x69uy -> Some (SongResponse (MDSong sysex)                                         )
    | 0x72uy -> Some (StatusResponse (MachineDrumStatusType.FromByte sysex.[7], sysex.[8]))
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
    match x with
    | InitSynthesis                     -> 0x0uy
    | InitSynthesisAndEffects           -> 0x1uy
    | InitSynthesisAndEffectsAndRouting -> 0x2uy

type MachineDrumSysexRequests =
| DumpGlobalSettings of globalSettingIndex: byte
| DumpKit of kit: byte
| DumpPattern of pattern: byte
| DumpSong of song: byte
| QueryStatus of statusType: MachineDrumStatusType
| LoadKit of kit: byte
| SaveKit of kit: byte
| SetCurrentKitName of string
| AssignMachine of track: Track * machine: MDMachineType * mode: AssignMachineMode
| SetReverbParameter of ReverbParameter * value: byte
with
  member x.MessageId = 
    match x with
    | DumpGlobalSettings _ -> 0x51uy
    | DumpKit _            -> 0x53uy
    | AssignMachine _      -> 0x5buy
    | DumpPattern _        -> 0x68uy
    | DumpSong _           -> 0x6auy
    | SetCurrentKitName _  -> 0x56uy
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
      | LoadKit kit            -> kit |> Array.singleton
      | SaveKit kit            -> kit |> Array.singleton
      | SetCurrentKitName name -> 
        name.PadRight(16)
        //|> System.Text.Encoding.ASCII.GetBytes 
        |> (fun s -> failwithf "TODO FABLE %s" s; [|0x0uy|])
        |> Seq.truncate 16 
        |> Seq.toArray
        |> Array.map byte
      | AssignMachine (track, machine, mode) ->
        let machineByte, uwByte =
          match machine with
          | MD m -> byte m, 0uy
          | MDUW m -> byte m, 1uy
        [|Track.trackValue track;machineByte;uwByte;mode.Value|]
      | SetReverbParameter(parameter, value) -> [|ReverbParameter.ToByte parameter;value|]
    Elektron.Platform.SysexHelper.makeMachineDrumSysexMessage (Array.concat ([|x.MessageId |> Array.singleton; data|]))

type LFOEvent =
| AssignTrack       of Track
| AssignDestination of MDTrackParameter
| AssignShape1      of LFOShape
| AssignShape2      of LFOShape
| AssignType        of LFOType

type MachineDrumEvent =
| TrackLevel        of Track * value: byte
| TrackParameter    of Track * MDTrackParameter * value: byte
| TrackTrigger      of Track * velocity: byte
| TrackRelease      of Track
| PatternChanged    of PatternLocator
| LFOSetting        of Track * LFOEvent
| DelaySetting      of DelayParameter * value: byte
| ReverbSetting     of ReverbParameter * value: byte
| EqualizerSetting  of EqualizerParameter * value: byte
| CompressorSetting of CompressorParameter * value: byte
| Unknown           of MidiMessage
| MachineDrumSysex  of MachineDrumSysexResponses
| Sysex             of byte array
| KitChanged        of byte
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


module Sysex =
  let mdHeader = [|
      0xf0uy
      0x00uy
      0x20uy
      0x3cuy
      0x02uy
      0x00uy
    |]

[<RequireQualifiedAccess>]
type MidiOutputData =
| Message of MidiMessage
| Sysex of bytes: byte array

type MachineDrum(inPort: IMidiInput<int>, outPort: IMidiOutput<int>, getSysexNowTimestamp: unit -> int) =
  let helpGetMDSysex maxMessage (timeout: TimeSpan) (request: MachineDrumSysexRequests) inPort : Async<MachineDrumSysexResponses option> =
  #if FABLE_COMPILER
    failwithf "TODO FABLE"
  #else
    Midi.Sysex.helpGetSysex maxMessage timeout (fun sysex -> sysex.[0..5] = Sysex.mdHeader && sysex.[6] = request.ResponseMessageId) (request.BuildResponse >> Option.get) inPort
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
              if isOn then
                  Some (MidiOutputData.Message (MidiMessage.NoteOn channel note velocity))
              else
                  Some (MidiOutputData.Message (MidiMessage.NoteOff channel note velocity))
      let makeCC track parameter value =
        let cc = MDTrackParameter.GetCCForTrack parameter track
        let channel = Track.midiBaseChannelOffset track + channel
        MidiMessage.CC channel cc value
        |> MidiOutputData.Message
      let makeProgramChange program =
        MidiMessage.ProgramChange channel program
        |> MidiOutputData.Message
        
      let none = Array.empty
      let some = Array.singleton
      match mdEvent with
      | TrackTrigger(track, velocity)            -> makeNote note velocity true  |> Option.get |> some
      | TrackRelease(track)                      -> makeNote note 0uy      false |> Option.get |> some
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
              x.EventToMidiMessages mdEvent globals 
              |> Array.iter (
                function
                | MidiOutputData.Message message -> outPort.WriteMessage (timestamp + now) message
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
    MidiMessage.CC (globals.MidiBaseChannel) cc value
    |> outPort.WriteMessage (getSysexNowTimestamp())
(*  
  member x.DumpKit kit =
    performSysExRequest (DumpKit kit)

  member x.DumpGlobalSettings id =
    performSysExRequest (DumpGlobalSettings id)
  member x.DumpPattern pattern =
    performSysExRequest (DumpPattern pattern)*)


module MachineDrumEventParser =
  let inline isMachineDrumChannel midiBaseChannel channel = channel >= midiBaseChannel && channel < midiBaseChannel + 4uy
  let inline isMachineDrumControlChange midiBaseChannel (message: MidiMessage) = 
    message.MessageType = MidiMessageType.ControllerChange 
    && isMachineDrumChannel midiBaseChannel message.Channel.Value
    && (message.Data1 >= 16uy && message.Data1 <= 119uy)
  let trackOffset cc =
    if   cc <= 39uy  then 0uy
    elif cc <= 63uy  then 1uy
    elif cc <= 95uy  then 2uy
    else                  3uy

type TimestampedMessage<'t> = { 
  Timestamp : int
  Message: 't
}

type MachineDrumEventListener(md: MachineDrum, getNow : unit -> int) =
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
          let cc = message.Data1
          let trackOffset = MachineDrumEventParser.trackOffset cc

          let midiChannelOffset = channel - midiBaseChannel
          let track = Track.trackForValue ((midiChannelOffset * 4uy) + trackOffset)
          let parameter = MDTrackParameter.GetParameterForCC cc
          TrackParameter (track, parameter, message.Data2)
        elif message.MessageType = MidiMessageType.ControllerChange && message.Data1 >= 8uy && message.Data1 <= 0xbuy then
          let midiChannelOffset = message.Channel.Value - midiBaseChannel
      
          let track =
            match message.Data1, midiChannelOffset with
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
          | Some track -> TrackLevel(track, message.Data2)
          | None -> Unknown message

        elif message.MessageType = MidiMessageType.NoteOn && message.Channel.Value = midiBaseChannel then
          let noteNumber = message.Data1
      
          match mdGlobalSettings.KeymapStructure.Triggers.[int noteNumber] with
          | NoteTriggerType.NoteTriggerType(_, TriggerChannel track) ->
            match message.Data2 with
            | 0uy      -> TrackRelease track
            | velocity -> TrackTrigger(track, velocity)
          | _ -> Unknown message
        else
          Unknown message

  let onSysexMessage (sysex: byte array) =
    match sysex with
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x5duy; paramIndex; paramValue; 0xf7uy|] ->
      DelaySetting(DelayParameter.FromByte paramIndex, paramValue)
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x5euy; paramIndex; paramValue; 0xf7uy|] ->
      ReverbSetting(ReverbParameter.FromByte paramIndex, paramValue)
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x5fuy; paramIndex; paramValue; 0xf7uy|] ->
      EqualizerSetting(EqualizerParameter.FromByte paramIndex, paramValue)
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x60uy; paramIndex; paramValue; 0xf7uy|] ->
      CompressorSetting(CompressorParameter.FromByte paramIndex, paramValue)
    | [|0xf0uy; 0x00uy; 0x20uy; 0x3cuy; 0x02uy; 0x00uy; 0x62uy; paramIndex; paramValue; 0xf7uy|] ->
      let lfoIndex = (paramIndex &&& 0b01111000uy) >>> 3
      let track = Track.trackForValue lfoIndex
      let lfoSettingIndex = paramIndex &&& 0b00000111uy
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

let mdDetection getTimestamp inputs outputs onSysex withMachineDrum  =
    let queryMessage = QueryStatus(GlobalSlot)
    let onSysex =
        match onSysex with 
        | Some onSysex -> onSysex 
        | _ -> 
        (fun sysex -> 
            match MachineDrumSysexResponses.BuildResponse sysex with
            | Some(MachineDrumSysexResponses.GlobalSettingsResponse globals) -> true
            | _ -> false        
        )
    
    Sysex.deviceInquiry inputs outputs
        onSysex
        (fun midiOut ->
            midiOut.WriteSysex 0 (QueryStatus(GlobalSlot).Sysex)
        )
        (fun midiIn midiOut ->
            let md = MachineDrum(midiIn, midiOut, getTimestamp)
            withMachineDrum md
            { new System.IDisposable with member x.Dispose () = () }
        )



let effectsParameters =
  [|
    ("AMD", "controls the the modulation depth")
    ("AMF", "controls the modulation frequency")
    ("EQF", "controls which center frequency that will be affected by the EQ gain")
    ("EQG", "controls the EQ gain. If set to a negative value, the volume of the frequency centered around the EQF parameter will be reduced")
    ("FLTF", "controls the base filter cutoff frequency")
    ("FLTW", "controls the filter gap width, that is the distance between the highpass and lowpass cutoff frequencies")
    ("FLTQ", "controls the filter quality Q parameter")
    ("SRR", "controls the amount of sample rate reduction")
  |]

let routingParameters =
  [|
    ("DIST", "parameter controls the signal overload distortion")
    ("VOL", "controls the volume of the track")
    ("PAN", "positions the sound in the stereo field")
    ("DEL", "controls the amount of signal that will be sent to the RHYTHM ECHO delay")
    ("REV", "controls how much signal will be sent to the Machinedrum GATE BOX reverb")
    ("LFOS", "controls the speed of the LFO")
    ("LFOD", "controls the modulation depth of the LFO")
    ("LFOM", "controls the mix between the two selectable LFO waveforms")
  |]
type SynthesisParameterName =
| Pitch
| Decay
| Ramp
| RampDecay
| Start
| Noise
| Harmonics
| Clip
| Hold
| Tick
| Dirt
| Distortion
| Bump
| BumpEnveloppe
| Snap
| Tone
| Tune
| Damp
| DistortionType
| Clappy
| Hard
| Richness
| Rate
| Room
| RoomSize
| RoomTuning
| Enhancement
| Gap
| HighpassFilter
| LowpassFilter
| Metal
| Top
| TopTune
| Size
| Peak
| Attack
| Sustain
| Reverse
| Rattle
| RattleType
| Dual
| Clic
| FMDepth
| FMFrequency
| FMDecay
| FMFeedback
| NoiseDecay
| Snare
| SnarePitch
| SnareDecay
| SnareModulationDepth
| Feedback
| Tremolo
| TremoloFrequency
| SnapLength
| SampleStart
| RetrigCount
| RetrigTime
| PitchBend
| HighPassFilterQ
| Ring
| Bell
| Stop
| Real
| BongoOrCongo
| Slew
| Hammer
| Tension
| Position
| GrainsCount
| GrainDecay
| Closeness
| AG
| AU
| BR
| Close
| Grab
| Up
| Down
| DownValue
| Volume
| GateSensitivity
| Level
| FilterAttack
| FilterHold
| FilterDecay
| FilterModulationDepth
| FilterFrequency
| FilterQ
| Amplitude
| Note
| Note2
| Note3
| Length
| Velocity
| ModulationWheel
| AfterTouch
| BitRateReduction
| End

type SynthesisParameterDescription = {
  Parameter: SynthesisParameterName
  Code: string
  Description: string
}

let synthesisParameters =
  let inline mkDesc p c d = {Parameter = p; Code = c; Description = d}
  let inline mkMdDesc mdMachine parameters = MD mdMachine, parameters
  let inline mkMdUwDesc mdMachine parameters = MDUW mdMachine, parameters

  let romParams = [|
    mkDesc Pitch            "PTCH" "Adjusts the pitch two octaves up or down. For the first octave up or down, every third parameter step adjusts the pitch one semi-note"
    mkDesc Decay            "DEC"  "The Decay time. If the sample is looped, a setting of 127 will make it loop infinitely"
    mkDesc Hold             "HOLD" "Hold the amplitude envelope open for the specified time. A value of 127 keeps the envelope open for 1 bar (16 steps). A value of 1 will keep the envelope open for 1/8 of a step"
    mkDesc BitRateReduction "BRR"  "Bit Rate Reduction. When set to 127 the sample will be of 2-bit quality"
    mkDesc Start            "STRT" "Controls the start point of the sample. The parameter is exponential, giving a very fine control over the beginning of a sample"
    mkDesc End              "END"  "Controls the end point of the sample. The sample will be reversed if it is set to a lower value than the STRT parameter"
    mkDesc RetrigCount      "RTRG" "Sets the number of times a sample will retrig. If set to 127 the sample will retrig infinitely"
    mkDesc RetrigTime       "RTIM" "Defines the time between two retrigs. The time is relative to the tempo. If the value is set to zero the RTRG parameter will not affect anything"
  |]

  [|
    mkMdDesc MDMachine.TRX_BD [|
      mkDesc Pitch     "PTCH" "Controls the basic pitch of the drum"
      mkDesc Decay     "DEC"  "Controls the decay time"
      mkDesc Ramp      "RAMP" "Ramps the pitch"
      mkDesc RampDecay "RDEC" "Speed of pitch ramp"
      mkDesc Start     "STRT" "Makes the start harder"
      mkDesc Noise     "NOIS" "Adds noise to the start of the sound"
      mkDesc Harmonics "HARM" "Adds extra harmonics"
      mkDesc Clip      "CLIP" "A special type of distortion"
    |]
    mkMdDesc MDMachine.TRX_B2 [|
      mkDesc Pitch      "PTCH" "Controls the basic pitch of the drum"
      mkDesc Decay      "DEC"  "Controls the decay time"
      mkDesc Ramp       "RAMP" "Ramps the pitch"
      mkDesc Hold       "HOLD" "Controls the initial hold time for the volume"
      mkDesc Tick       "TICK" "Adds a tick to make the start harder"
      mkDesc Noise      "NOIS" "Adds noise to the start of the sound"
      mkDesc Dirt       "DIRT" "Controls a bit reduction function for the bassdrum"
      mkDesc Distortion "DIST" "A special type of distortion"
    |]
    mkMdDesc MDMachine.TRX_SD [|
      mkDesc Pitch         "PTCH" "Controls the basic pitch of the drum"
      mkDesc Decay         "DEC"  "Controls the decay time"
      mkDesc Bump          "BUMP" "Adds a pitch shift at the start"
      mkDesc BumpEnveloppe "BENV" "Controls the envelope of the bump"
      mkDesc Snap          "SNAP" "Amount of snap"
      mkDesc Tone          "TONE" "Changes tonal quality"
      mkDesc Tune          "TUNE" "Detunes the drum"
      mkDesc Clip          "CLIP" "A special type of distortion"
    |]
    mkMdDesc MDMachine.TRX_XT [|
      mkDesc Pitch          "PTCH" "Controls the basic pitch of the drum"
      mkDesc Decay          "DEC"  "Controls the decay time"
      mkDesc Ramp           "RAMP" "Ramps the pitch"
      mkDesc RampDecay      "RDEC" "Speed of pitch ramp"
      mkDesc Damp           "DAMP" "Dampens the decay"
      mkDesc Distortion     "DIST" "Distortion"
      mkDesc DistortionType "DTYP" "Hardness of the distortion"
    |]
    mkMdDesc MDMachine.TRX_CP [|
      mkDesc Clappy     "CLPY" "Density of the clap"
      mkDesc Tone       "TONE" "Changes tonal quality"
      mkDesc Hard       "HARD" "Harder claps"
      mkDesc Richness   "RICH" "Enhances the richness of the sound"
      mkDesc Rate       "RATE" "Rate of the individual claps"
      mkDesc Room       "ROOM" "Adds a room sound"
      mkDesc RoomSize   "RSIZ" "Changes the size of the room"
      mkDesc RoomTuning "RTUN" "Changes the tonal quality of the room"
    |]
    mkMdDesc MDMachine.TRX_RS [|
      mkDesc Pitch      "PTCH" "Controls the basic pitch"
      mkDesc Decay      "DEC"  "Controls the decay time"
      mkDesc Distortion "DIST" "Distortion"
    |]
    mkMdDesc MDMachine.TRX_CB [|
      mkDesc Pitch       "PTCH" "Controls the basic pitch"
      mkDesc Decay       "DEC"  "Controls the decay time"
      mkDesc Enhancement "ENH"  "Enhances the body of the sound"
      mkDesc Damp        "DAMP" "Dampens the decay part"
      mkDesc Tone        "TONE" "Changes tonal quality"
      mkDesc Bump        "BUMP" "Adds a pitchshift at the start of the sound"
    |]
    mkMdDesc MDMachine.TRX_CH [|
      mkDesc Gap            "GAP"  "Changes the hi-hat gap"
      mkDesc Decay          "DEC"  "Controls the decay time"
      mkDesc HighpassFilter "HPF"  "High pass filters the sound"
      mkDesc LowpassFilter  "LPF"  "Low pass filters the sound"
      mkDesc Metal          "MTAL" "Adds extra metal character to the sound"
    |]
    mkMdDesc MDMachine.TRX_OH [|
      mkDesc Gap            "GAP"  "Changes the hi-hat gap"
      mkDesc Decay          "DEC"  "Controls the decay time"
      mkDesc HighpassFilter "HPF"  "High pass filters the sound"
      mkDesc LowpassFilter  "LPF"  "Low pass filters the sound"
      mkDesc Metal          "MTAL" "Adds extra metal character to the sound"
    |]
    mkMdDesc MDMachine.TRX_CY [|
      mkDesc Richness "RICH" "Adds extra richness to the sound"
      mkDesc Decay    "DEC"  "Controls the decay time"
      mkDesc Top      "TOP"  "Amount of high frequencies harmonics"
      mkDesc TopTune  "TTUN" "Tunes the top"
      mkDesc Size     "SIZE" "Changes the size of the cymbal"
      mkDesc Peak     "PEAK" "Gives the sound more edge"
    |]
    mkMdDesc MDMachine.TRX_MA [|
      mkDesc Attack     "ATT"  "Length of the attack"
      mkDesc Sustain    "SUS"  "Length of the sustain"
      mkDesc Reverse    "REV"  "Reverses movement of the maracas"
      mkDesc Damp       "DAMP" "Dampens the sound to make it more sparse"
      mkDesc Rattle     "RATL" "Adds extra rattle to the sound"
      mkDesc RattleType "RTYP" "Type of rattle"
      mkDesc Tone       "TONE" "Changes tonal quality"
      mkDesc Hard       "HARD" "Gives the sound a harder character"
    |]
    mkMdDesc MDMachine.TRX_CL [|
      mkDesc Pitch       "PTCH" "Controls the basic pitch"
      mkDesc Decay       "DEC"  "Controls the decay time"
      mkDesc Dual        "DUAL" "Introduces a dual attack"
      mkDesc Enhancement "ENH"  "Enhances the tone"
      mkDesc Tune        "TUNE" "Detunes the sound"
      mkDesc Clic        "CLIC" "Adds a click at the start"
    |]
    mkMdDesc MDMachine.TRX_XC [|
      mkDesc Pitch          "PTCH" "Controls the basic pitch of the drum"
      mkDesc Decay          "DEC"  "Controls the decay time"
      mkDesc Ramp           "RAMP" "Ramps the pitch"
      mkDesc RampDecay      "RDEC" "Speed of the pitch ramp"
      mkDesc Damp           "DAMP" "Dampens the decay part of the sound"
      mkDesc Distortion     "DIST" "Distortion"
      mkDesc DistortionType "DTYP" "Hardness of the distortion"
    |]
    mkMdUwDesc MDUWMachine.ROM_01 romParams
    mkMdUwDesc MDUWMachine.ROM_02 romParams
    mkMdUwDesc MDUWMachine.ROM_03 romParams
    mkMdUwDesc MDUWMachine.ROM_04 romParams
    mkMdUwDesc MDUWMachine.ROM_05 romParams
    mkMdUwDesc MDUWMachine.ROM_06 romParams
    mkMdUwDesc MDUWMachine.ROM_07 romParams
    mkMdUwDesc MDUWMachine.ROM_08 romParams
    mkMdUwDesc MDUWMachine.ROM_09 romParams
    mkMdUwDesc MDUWMachine.ROM_10 romParams
    mkMdUwDesc MDUWMachine.ROM_11 romParams
    mkMdUwDesc MDUWMachine.ROM_12 romParams
    mkMdUwDesc MDUWMachine.ROM_13 romParams
    mkMdUwDesc MDUWMachine.ROM_14 romParams
    mkMdUwDesc MDUWMachine.ROM_15 romParams
    mkMdUwDesc MDUWMachine.ROM_16 romParams
    mkMdUwDesc MDUWMachine.ROM_17 romParams
    mkMdUwDesc MDUWMachine.ROM_18 romParams
    mkMdUwDesc MDUWMachine.ROM_19 romParams
    mkMdUwDesc MDUWMachine.ROM_20 romParams
    mkMdUwDesc MDUWMachine.ROM_21 romParams
    mkMdUwDesc MDUWMachine.ROM_22 romParams
    mkMdUwDesc MDUWMachine.ROM_23 romParams
    mkMdUwDesc MDUWMachine.ROM_24 romParams
    mkMdUwDesc MDUWMachine.ROM_25 romParams
    mkMdUwDesc MDUWMachine.ROM_26 romParams
    mkMdUwDesc MDUWMachine.ROM_27 romParams
    mkMdUwDesc MDUWMachine.ROM_28 romParams
    mkMdUwDesc MDUWMachine.ROM_29 romParams
    mkMdUwDesc MDUWMachine.ROM_30 romParams
    mkMdUwDesc MDUWMachine.ROM_31 romParams
    mkMdUwDesc MDUWMachine.ROM_32 romParams
    mkMdUwDesc MDUWMachine.ROM_33 romParams
    mkMdUwDesc MDUWMachine.ROM_34 romParams
    mkMdUwDesc MDUWMachine.ROM_35 romParams
    mkMdUwDesc MDUWMachine.ROM_36 romParams
    mkMdUwDesc MDUWMachine.ROM_37 romParams
    mkMdUwDesc MDUWMachine.ROM_38 romParams
    mkMdUwDesc MDUWMachine.ROM_39 romParams
    mkMdUwDesc MDUWMachine.ROM_40 romParams
    mkMdUwDesc MDUWMachine.ROM_41 romParams
    mkMdUwDesc MDUWMachine.ROM_42 romParams
    mkMdUwDesc MDUWMachine.ROM_43 romParams
    mkMdUwDesc MDUWMachine.ROM_44 romParams
    mkMdUwDesc MDUWMachine.ROM_45 romParams
    mkMdUwDesc MDUWMachine.ROM_46 romParams
    mkMdUwDesc MDUWMachine.ROM_47 romParams
    mkMdUwDesc MDUWMachine.ROM_48 romParams
    mkMdUwDesc MDUWMachine.ROM_48 romParams
    mkMdUwDesc MDUWMachine.RAM_P1 romParams
    mkMdUwDesc MDUWMachine.RAM_P2 romParams
    mkMdUwDesc MDUWMachine.RAM_P3 romParams
    mkMdUwDesc MDUWMachine.RAM_P4 romParams
  |] |> dict
