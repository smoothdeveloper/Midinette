namespace Midi
open FSharp.UMX
type [<Measure>] private bits7'
type [<Measure>] private bits8'
type [<Measure>] private bits14'
type [<Measure>] private bits16'
type [<Measure>] private sysex'

type sysex_data   = byte<sysex'> array
type byte_7bits   = byte<bits7'>
type byte_8bits   = byte<bits8'>
type short_14bits = byte<bits7'>

type UMX =
  static member inline untag_byte_7bits (byte: byte_7bits)   : byte = unbox byte
  static member inline untag_sysex      (byte: byte<'sysex>) : byte = unbox byte
  static member inline untag_sysex      (bytes: sysex_data) : byte array = unbox bytes

  static member inline tag_byte_7bits (byte: byte option) : byte_7bits option = unbox byte
  static member inline tag_byte_7bits (byte: byte)        : byte_7bits       = unbox byte
  static member inline tag_byte_7bits (bytes: byte array) : byte_7bits array = unbox bytes
  static member inline tag_sysex_data (bytes: byte option) : byte<sysex'> option = unbox bytes
  static member inline tag_sysex_data (bytes: byte array) : sysex_data       = unbox bytes
  static member inline tag_sysex_data (bytes: byte array array) : sysex_data array = unbox bytes
  static member inline tag_sysex_data (byte: byte)        : byte<sysex'>     = unbox byte
  static member inline tag_sysex_data (byte: Event<byte array>) : Event<byte<sysex'>> = unbox byte
  
  static member inline to_byte_7bits(sysex: sysex_data)    : byte_7bits array = unbox sysex
  static member inline to_byte_7bits(sysex: byte<_>)       : byte_7bits       = unbox sysex
  static member inline to_sysex_data(sysex: byte<_>)       : byte<sysex'>     = unbox sysex
  static member inline to_sysex_data(sysex: byte<_> array) : sysex_data       = unbox sysex
