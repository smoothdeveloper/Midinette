namespace Midinette.Waldorf
open FSharp.UMX
open Midi

module Synthesizers =
    type [<Measure>] microwave1osversion
    type Microwave1OsVersion = decimal<microwave1osversion>
    
    type Microwave2Variant =
        | Microwave2
        | XTRack
        | XTKeyboard
    
    type Device =
        | Microwave1 of Microwave1OsVersion
        | Microwave2 of Microwave2Variant
        
    type ValueNameMapping = obj
    type ICCParameter =
        abstract Index : byte<midicc>
        abstract ValueNameMapping : ValueNameMapping option
      
    type ParamRange = ZeroTo127
    let CCParams = [
        1  , (ZeroTo127), "Modulation wheel MSB   Glide Time"
        2  , (ZeroTo127), "Breath control MSB" // ""Channel Volume Panning left"
        4  , (ZeroTo127), "Foot controller MSB"
        5  , (ZeroTo127), "Glide Time"
        7  , (ZeroTo127), "Channel Volume"
        12 , (ZeroTo127), "Chorus 0:off 1:on"
        14 , (ZeroTo127), "Filter Env Attack" 
        15 , (ZeroTo127), "Filter Env Decay"
        16 , (ZeroTo127), "Filter Env Sustain"
        17 , (ZeroTo127), "Filter Env Release"
        18 , (ZeroTo127), "Amp Env Attack"
        19 , (ZeroTo127), "Amp Env Decat"
        20 , (ZeroTo127), "Amp Env Sustain"
        21 , (ZeroTo127), "Amp Env Release"
        22 , (ZeroTo127), "Glide Type"
        23 , (ZeroTo127), "Glide Mode"
        24 , (ZeroTo127), "LFO1 Rate"
        25 , (ZeroTo127), "LFO1 Shape"
        26 , (ZeroTo127), "LFO2 Rate"
        27 , (ZeroTo127), "LFO2 Delay"
        28 , (ZeroTo127), "LFO2 Shape"
        29 , (ZeroTo127), "Filter Env Trigger"
        30 , (ZeroTo127), "LFO1 Delay"
        31 , (ZeroTo127), "Amp Env Trigger"
        32 , (ZeroTo127), "Bank Select"
        33 , (ZeroTo127), "Ost 1 Octave"
        34 , (ZeroTo127), "Osc 1 Semitone"
        35 , (ZeroTo127), "Osc 1 Detune"
        36 , (ZeroTo127), "Osc 1 Pitchbend Scale"
        122, (ZeroTo127), ""
        37 , (ZeroTo127), "Osc 1 Keytrack"
        38 , (ZeroTo127), "Osc 2 Octave"
        39 , (ZeroTo127), "Osc 2 Semitone"
        40 , (ZeroTo127), "Osc 2 Detune"
        41 , (ZeroTo127), "Osc 2 Sync"
        42 , (ZeroTo127), "Osc 2 Picthbend Scale" 
        43 , (ZeroTo127), "Osc 2 Keytrack"
        44 , (ZeroTo127), "Osc 2 Link"
        45 , (ZeroTo127), "Wave 1 Level"
        46 , (ZeroTo127), "Wave 2 Level"
        47 , (ZeroTo127), "RingMod Level"
        48 , (ZeroTo127), "Noise Level"
        50 , (ZeroTo127), "Filter 1 Cutoff"
        51 , (ZeroTo127), "Filter 1 Keytrack"
        52 , (ZeroTo127), "Filter 1 Env Amount"
        53 , (ZeroTo127), "Filter 1 Env Velocity"
        54 , (ZeroTo127), "Filter 1 Type"
        55 , (ZeroTo127), "Amp Keytrack"
        56 , (ZeroTo127), "Filter 1 Resonance"
        57 , (ZeroTo127), "Amp Volume"
        58 , (ZeroTo127), "Amp Env Velocity"
        60 , (ZeroTo127), "Filter 2 Cutoff"
        61 , (ZeroTo127), "Filter 2 Type"
        62 , (ZeroTo127), "Filter 2 Keytrack"
        64 , (ZeroTo127), "Sustain Switch"
        65 , (ZeroTo127), "Sustain Switch"
        66 , (ZeroTo127), "Sustain Switch"
        70 , (ZeroTo127), "Sustain Switch"
        71 , (ZeroTo127), "Sustain Switch"
        72 , (ZeroTo127), "Sustain Switch"
        73 , (ZeroTo127), "Sustain Switch"
        74 , (ZeroTo127), "Sustain Switch"
        75 , (ZeroTo127), "Sustain Switch"
        76 , (ZeroTo127), "Sustain Switch"
        77 , (ZeroTo127), "Sustain Switch"
        78 , (ZeroTo127), "Sustain Switch"
        80 , (ZeroTo127), "Sustain Switch"
        81 , (ZeroTo127), "Sustain Switch"
        82 , (ZeroTo127), "Sustain Switch"
        83 , (ZeroTo127), "Sustain Switch"
        85 , (ZeroTo127), "Sustain Switch"
    ]  
    
      
        
    (*
    Waldorf Microwave 2/XT Controller Number Assignment Software release 2.09
Contr. No. Range Parameter Value Range
-------------------------------------------------------------------------------- ------
1  0..127
2  0..127
4  0..127
5  0..127
7  0..127 10 0..127
12 0..1
14 0..127
15 0..127
16 0..127
17 0..127
18 0..127
19 0..127
20 0..127
21 0..127
22 0..3
23 0..1
24 0..127
25 0..5
26 0..127
27 0..127
28 0..5
29 0..2
30 0..127
31 0..2
32 0..1
33 16..112
34 56..76
35 0..127
36 0..121
122:global
37 0..127
38 16..112
39 56..76
40 0..127
41 0..1
42 0..121
43 0..127
44 0..1
45 0..127
46 0..127
47 0..127
Modulation wheel MSB Breath control MSB Foot controller MSB Glide Time
Channel Volume Panning left
Chorus 0:off 1:on
Filter Env Attack Filter Env Decay Filter Env Sustain Filter Env Release Amp Env Attack
Amp Env Decay Amp Env Sustain Amp Env Release
Glide Type
Glide Mode LFO1 Rate LFO1 Shape
LFO2 Rate LFO2 Delay LFO2 Shape
Filter Env
LFO1 Delay
Amp Env Trigger
Bank Select
Osc 1 Octave
Osc 1 Semitone
Osc 1 Detune
Osc 1 Pitchbend Scale
Osc 1 Keytrack Osc 2 Octave Osc 2 Semitone Osc 2 Detune Osc 2 Sync
Osc 2 Pitchbend Scale
Osc 2 Keytrack Osc 2 Link
Wave 1 Level Wave 2 Level RingMod Level
0..127
0..127
0..127
0..127
0..127 64..center..right 63
0..127
0..127
0..127
0..127
0..127
0..127
0..127
0..127
0:portamento 1:fingered port. 2:glissando 3:fingered gliss. 0:exp. 1:linear
0..127
0:sin 1:tri 2:square 3:saw
5:S&H
0..127
0:off 1:retrigger 2..127:1..126 0:sin 1:tri 2:square 3:saw
4:random
4:random
Trigger
5:S&H
0:normal 1:single 0:off 1:retrigger 0:normal 1:single
0:Bank A 1:Bank B
2:retrigger 2..127:1..126 2:retrigger
-4..+4 (in steps of 12) -12..+12
-64..+63
0..120:semitones 121:harmonic
-100%..+200%
-4..+4 (in steps of 12) -12..+12
-64..+63
0:off 1:on
0..120:semitones 121:harmonic 122:global
-100%..+200%
0:off 1:on
0..127
0..127
0..127
Filter
48 0..127
Noise Level
0..127
50 0..127
51 0..127
52 0..127
53 0..127
54 0..9
55 0..127
56 0..127
57 0..127
58 0..127
60 0..127
61 0..1
62 0..127
64 0..127
65 0..127
66 0..127
70 0..127
71 0..63
72 0..127
73 0..127
74 0..127
75 0..127
76 0..1
77 0..63
78 0..127
79 0..127
80 0..127
81 0..127
82 0..1
83 0..1
85 0..127
86 0..127
87 0..127
88 0..127
89 0..127
90 0..127
91 0..127
92 0..127
93 0..2
102 0..2
103 0..9
104 0..15
105 0..127
106 0..3
107 0..16
108 0..3
109 0..1
110 0..1
111 0..15
Filter 1 Cutoff Filter 1 Keytrack Filter 1 Env Amount Filter 1 Env Velocity Filter 1 Type
Amp Keytrack Filter 1 Resonance Amp Volume
Amp Env Velocity Filter 2 Cutoff Filter 2 Type Filter 2 Keytrack
Sustain Switch 0..127 Portamento Switch Sostenuto Switch
Wavetable
Wave 1 Startwave
Wave 1 Phase
Wave 1 Env Amnt.
Wave 1 Env Vel. Amnt. Wave 1 Keytrack
Wave 1 Limit
Wave 2 Startwave
0..127
-200%..+197%
-64..+63
-64..+63
0:24dB LP 1:12dB LP 2:24dB BP 3:12dB BP 4:12dB HP 5:sinX>12L
Wave 2 Phase Wave 2 Env Amnt. Wave 2 Env Vel. Wave 2 Keytrack Wave 2 Limit Wave 2 Link
Free Env Time 1 Free Env Level 1 Free Env Time 2 Free Env Level 2 Free Env Time 3 Free Env Level 3 Free Env Release Free Env Release Free Env Trigger
Arp Active
Arp Range
Arp Clock
Arp Tempo
Arp Direction Arp Pattern Arp Note Order
0:free 1..127:3..357 degree -64..+63
Arp Velocity
Arp Reset
Arp Pattern Length
0:off 1:on 2:hold
1..10 Octaves
1/1..1/32
0:external 1..127:50..300BPM 0:up 1:down 2:alternate 3:random 0:off 1:user 2..16:Pattern 1..15 0:by note 1:note rev 2:as played 3:reversed
0:root note 1:last note 0:off 1:on
1..16
Amnt.
Time Level
-64..+63 -200%..+197% 0:off 1:on 0:off 1:on
6:Waveshaper 7:Dual
9:S&H->L12dB -200%..+197% 0..127 0..127 -64..+63 0..127
0:6dB LP 1:6dB HP -200%..+197%
0..127
0..127
8:FM-
Wavetable 001..128
00..60 61:triangle 62:square 63:saw
0:free 1..127:3..357 degree -64..+63
-64..+63 -200%..+197%
0:off 1:on
00..60 61:triangle 62:square 63:saw
0..127 -64..+63 0..127 -64..+63 0..127 -64..+63 0..127 -64..+63 0:normal
1:single 2:retrigger

112 0..1
113 0..127
114 0..127
115 0..1
116 0..127
117 0..127
118 0..127
120 0
121 0
123 0
LFO 1 Sync
LFO 1 Symmetry LFO 1 Humanize LFO 2 Sync
LFO 2 Symmetry LFO 2 Humanize LFO 2 Phase
All Sound Off
Reset All Controllers All notes off
0:off 1:on
-64..+63
0..127
0:off 1:on
-64..+63
0..127
0:free 1..127:3..357 degree
    *)
    //let getControlChangeParameters (device: Device) : ICCParameter seq = failwith ""