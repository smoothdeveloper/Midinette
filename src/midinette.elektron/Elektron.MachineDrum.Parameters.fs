module Elektron.MachineDrum.Parameters

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
  |> Array.zip MDTrackParameter.effectsParameters

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
  |> Array.zip MDTrackParameter.routingParameters
  
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
| Unmapped of byte //<bit3>

type SynthesisParameterDescription = {
  Parameter: SynthesisParameterName
  Code: string
  Description: string
}

let mkDesc p c d = {Parameter = p; Code = c; Description = d}
let mkMdDesc mdMachine parameters = MD mdMachine, parameters
let mkMdUwDesc mdMachine parameters = MDUW mdMachine, parameters

let synthesisParameters =

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
