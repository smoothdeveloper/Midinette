/// Raw C ABI of `native/midinette-midir-ffi` (Rust, midir). Blittable signatures only.
module internal midinette.platform.midir.Native
#nowarn "9"   // native pointers
#nowarn "51"  // address-of
open System
open System.Runtime.InteropServices
open System.Text
open Microsoft.FSharp.NativeInterop

[<Literal>]
let Lib = "midinette_midir"
[<Literal>]
let ExpectedAbiVersion = 1u

let [<Literal>] Ok = 0
let [<Literal>] ErrNotFound = -2
let [<Literal>] ErrUnsupported = -3

[<DllImport(Lib)>] extern uint32 mmidir_abi_version()
[<DllImport(Lib)>] extern int mmidir_last_error(nativeint buf, unativeint cap)
[<DllImport(Lib)>] extern int mmidir_supports_virtual_ports()
[<DllImport(Lib)>] extern int mmidir_backend_name(nativeint buf, unativeint cap)
[<DllImport(Lib)>] extern int mmidir_list_ports(int direction, nativeint buf, unativeint cap)
[<DllImport(Lib)>] extern int mmidir_input_open(nativeint id, unativeint idLen, nativeint client, unativeint clientLen, nativeint cb, nativeint userData, nativeint* out)
[<DllImport(Lib)>] extern int mmidir_input_create_virtual(nativeint name, unativeint nameLen, nativeint cb, nativeint userData, nativeint* out)
[<DllImport(Lib)>] extern int mmidir_input_close(nativeint handle)
[<DllImport(Lib)>] extern int mmidir_output_open(nativeint id, unativeint idLen, nativeint client, unativeint clientLen, nativeint* out)
[<DllImport(Lib)>] extern int mmidir_output_create_virtual(nativeint name, unativeint nameLen, nativeint* out)
[<DllImport(Lib)>] extern int mmidir_output_send(nativeint handle, nativeint data, unativeint len)
[<DllImport(Lib)>] extern int mmidir_output_close(nativeint handle)

/// call a "write into buffer, return needed length" function and decode UTF-8
let readString (f: nativeint -> unativeint -> int) =
  let needed = f 0n 0un
  if needed < 0 then Error needed
  elif needed = 0 then Result.Ok ""
  else
    let buffer = Array.zeroCreate<byte> needed
    use p = fixed buffer
    let n = f (NativePtr.toNativeInt p) (unativeint buffer.Length)
    if n < 0 then Error n
    else Result.Ok (Encoding.UTF8.GetString(buffer, 0, min n buffer.Length))

let lastError () =
  match readString (fun b c -> mmidir_last_error(b, c)) with
  | Result.Ok s -> s
  | Error _ -> "unknown midir error"

/// pass a string as UTF-8 (ptr, len) to `f`
let withUtf8 (s: string) (f: nativeint -> unativeint -> 'r) =
  let bytes = Encoding.UTF8.GetBytes s
  if bytes.Length = 0 then f 0n 0un
  else
    use p = fixed bytes
    f (NativePtr.toNativeInt p) (unativeint bytes.Length)

let withBytes (bytes: byte array) (f: nativeint -> unativeint -> 'r) =
  if bytes.Length = 0 then f 0n 0un
  else
    use p = fixed bytes
    f (NativePtr.toNativeInt p) (unativeint bytes.Length)
