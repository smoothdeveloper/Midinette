//! C ABI shim over `midir` for Midinette (.NET side: `midinette.platform.midir`).
//!
//! Conventions
//! - every fallible function returns an `i32` status: `0` = ok, negative = error
//!   (details via `mmidir_last_error`, thread-local).
//! - strings in: UTF-8 pointer + length; strings out: caller buffer + capacity,
//!   the function returns the full byte length needed (call again with a bigger buffer).
//! - no panic crosses the boundary (`catch_unwind` everywhere, incl. the input callback).
//! - input callback runs on midir's thread: `cb(user_data, timestamp_us, data, len)`;
//!   it must copy `data` and return quickly. After `mmidir_input_close` returns, the
//!   callback is never invoked again for that connection.

use std::cell::RefCell;
use std::os::raw::c_void;
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::sync::{Arc, Mutex, MutexGuard};

use midir::{Ignore, MidiInput, MidiInputConnection, MidiOutput, MidiOutputConnection};

pub const MMIDIR_OK: i32 = 0;
pub const MMIDIR_ERR: i32 = -1;
pub const MMIDIR_ERR_NOT_FOUND: i32 = -2;
pub const MMIDIR_ERR_UNSUPPORTED: i32 = -3;
pub const MMIDIR_ERR_INVALID_ARG: i32 = -4;
pub const MMIDIR_ERR_PANIC: i32 = -99;

/// ABI version, bump on any breaking change of the exported surface.
pub const MMIDIR_ABI_VERSION: u32 = 1;

pub type InputCallback =
    extern "C" fn(user_data: *mut c_void, timestamp_us: u64, data: *const u8, len: usize);

thread_local! {
    static LAST_ERROR: RefCell<String> = RefCell::new(String::new());
}

fn set_error<S: Into<String>>(s: S) {
    LAST_ERROR.with(|e| *e.borrow_mut() = s.into());
}

fn guard<F: FnOnce() -> i32>(f: F) -> i32 {
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(code) => code,
        Err(_) => {
            set_error("panic in midinette-midir-ffi");
            MMIDIR_ERR_PANIC
        }
    }
}

unsafe fn str_arg<'a>(ptr: *const u8, len: usize) -> Option<&'a str> {
    if ptr.is_null() {
        return if len == 0 { Some("") } else { None };
    }
    std::str::from_utf8(std::slice::from_raw_parts(ptr, len)).ok()
}

unsafe fn write_out(s: &str, buf: *mut u8, cap: usize) -> i32 {
    let bytes = s.as_bytes();
    if !buf.is_null() && cap > 0 {
        let n = bytes.len().min(cap);
        std::ptr::copy_nonoverlapping(bytes.as_ptr(), buf, n);
    }
    bytes.len() as i32
}

// ---------------------------------------------------------------- meta

#[no_mangle]
pub extern "C" fn mmidir_abi_version() -> u32 {
    MMIDIR_ABI_VERSION
}

/// Writes the last error of the calling thread; returns its byte length.
#[no_mangle]
pub unsafe extern "C" fn mmidir_last_error(buf: *mut u8, cap: usize) -> i32 {
    LAST_ERROR.with(|e| write_out(&e.borrow(), buf, cap))
}

/// 1 when this build/OS can create virtual ports (macOS CoreMIDI, Linux ALSA/JACK), 0 otherwise.
#[no_mangle]
pub extern "C" fn mmidir_supports_virtual_ports() -> i32 {
    if cfg!(unix) { 1 } else { 0 }
}

/// Name of the midir backend compiled in (e.g. "coremidi", "alsa", "winmm").
#[no_mangle]
pub unsafe extern "C" fn mmidir_backend_name(buf: *mut u8, cap: usize) -> i32 {
    let name = if cfg!(target_os = "macos") || cfg!(target_os = "ios") {
        "coremidi"
    } else if cfg!(all(target_os = "linux", feature = "jack")) {
        "jack"
    } else if cfg!(target_os = "linux") {
        "alsa"
    } else if cfg!(all(windows, feature = "winrt")) {
        "winrt"
    } else if cfg!(windows) {
        "winmm"
    } else {
        "unknown"
    };
    write_out(name, buf, cap)
}

// ---------------------------------------------------------------- enumeration
// direction: 0 = inputs (sources we read from), 1 = outputs (destinations we write to)

fn port_list(direction: i32) -> Result<Vec<(String, String)>, String> {
    match direction {
        0 => {
            let mi = MidiInput::new("midinette-enum").map_err(|e| e.to_string())?;
            Ok(mi
                .ports()
                .iter()
                .map(|p| (p.id(), mi.port_name(p).unwrap_or_default()))
                .collect())
        }
        1 => {
            let mo = MidiOutput::new("midinette-enum").map_err(|e| e.to_string())?;
            Ok(mo
                .ports()
                .iter()
                .map(|p| (p.id(), mo.port_name(p).unwrap_or_default()))
                .collect())
        }
        _ => Err(format!("invalid direction {direction}")),
    }
}

/// Snapshot of the port list, as a single UTF-8 string:
/// one line per port, `id \t name \n`. Returns needed byte length (or < 0 on error).
#[no_mangle]
pub unsafe extern "C" fn mmidir_list_ports(direction: i32, buf: *mut u8, cap: usize) -> i32 {
    guard(|| match port_list(direction) {
        Ok(ports) => {
            let mut s = String::new();
            for (id, name) in ports {
                s.push_str(&id.replace(['\t', '\n'], " "));
                s.push('\t');
                s.push_str(&name.replace(['\t', '\n'], " "));
                s.push('\n');
            }
            write_out(&s, buf, cap)
        }
        Err(e) => {
            set_error(e);
            MMIDIR_ERR
        }
    })
}

// ---------------------------------------------------------------- input

pub struct InputHandle {
    conn: Option<MidiInputConnection<()>>,
    /// `true` while the managed side may be called; cleared (under the lock) by close.
    alive: Arc<Mutex<bool>>,
}

struct CallbackTarget {
    cb: InputCallback,
    user_data: usize,
    alive: Arc<Mutex<bool>>,
}

fn lock_ignore_poison<T>(m: &Mutex<T>) -> MutexGuard<'_, T> {
    match m.lock() {
        Ok(g) => g,
        Err(p) => p.into_inner(),
    }
}

fn make_callback(target: CallbackTarget) -> impl FnMut(u64, &[u8], &mut ()) + Send + 'static {
    move |ts, data, _| {
        // the managed callback only runs while holding `alive`: once close has cleared it,
        // an in-flight or late OS callback can never reach a freed GCHandle.
        let alive = lock_ignore_poison(&target.alive);
        if !*alive {
            return;
        }
        let cb = target.cb;
        let ud = target.user_data as *mut c_void;
        let _ = catch_unwind(AssertUnwindSafe(|| cb(ud, ts, data.as_ptr(), data.len())));
    }
}

fn new_input(client_name: &str) -> Result<MidiInput, String> {
    let mut mi = MidiInput::new(client_name).map_err(|e| e.to_string())?;
    // midinette wants everything: sysex, timing clock, active sensing.
    mi.ignore(Ignore::None);
    Ok(mi)
}

/// Opens the input port with `id` (from `mmidir_list_ports(0, ..)`).
#[no_mangle]
pub unsafe extern "C" fn mmidir_input_open(
    id_ptr: *const u8,
    id_len: usize,
    client_name_ptr: *const u8,
    client_name_len: usize,
    cb: Option<InputCallback>,
    user_data: *mut c_void,
    out: *mut *mut InputHandle,
) -> i32 {
    guard(|| {
        let (Some(id), Some(client), Some(cb)) = (
            str_arg(id_ptr, id_len),
            str_arg(client_name_ptr, client_name_len),
            cb,
        ) else {
            set_error("invalid argument");
            return MMIDIR_ERR_INVALID_ARG;
        };
        if out.is_null() {
            set_error("out is null");
            return MMIDIR_ERR_INVALID_ARG;
        }
        let mi = match new_input(client) {
            Ok(m) => m,
            Err(e) => {
                set_error(e);
                return MMIDIR_ERR;
            }
        };
        let Some(port) = mi.find_port_by_id(id) else {
            set_error(format!("input port not found: {id}"));
            return MMIDIR_ERR_NOT_FOUND;
        };
        let alive = Arc::new(Mutex::new(true));
        let target = CallbackTarget { cb, user_data: user_data as usize, alive: alive.clone() };
        match mi.connect(&port, "midinette-in", make_callback(target), ()) {
            Ok(conn) => {
                *out = Box::into_raw(Box::new(InputHandle { conn: Some(conn), alive }));
                MMIDIR_OK
            }
            Err(e) => {
                set_error(e.to_string());
                MMIDIR_ERR
            }
        }
    })
}

/// Creates a virtual input (a destination other apps can write to). Unix only.
#[no_mangle]
pub unsafe extern "C" fn mmidir_input_create_virtual(
    name_ptr: *const u8,
    name_len: usize,
    cb: Option<InputCallback>,
    user_data: *mut c_void,
    out: *mut *mut InputHandle,
) -> i32 {
    guard(|| {
        let (Some(name), Some(cb)) = (str_arg(name_ptr, name_len), cb) else {
            set_error("invalid argument");
            return MMIDIR_ERR_INVALID_ARG;
        };
        if out.is_null() {
            set_error("out is null");
            return MMIDIR_ERR_INVALID_ARG;
        }
        #[cfg(unix)]
        {
            use midir::os::unix::VirtualInput;
            let mi = match new_input(name) {
                Ok(m) => m,
                Err(e) => {
                    set_error(e);
                    return MMIDIR_ERR;
                }
            };
            let alive = Arc::new(Mutex::new(true));
        let target = CallbackTarget { cb, user_data: user_data as usize, alive: alive.clone() };
            match mi.create_virtual(name, make_callback(target), ()) {
                Ok(conn) => {
                    *out = Box::into_raw(Box::new(InputHandle { conn: Some(conn), alive }));
                    MMIDIR_OK
                }
                Err(e) => {
                    set_error(e.to_string());
                    MMIDIR_ERR
                }
            }
        }
        #[cfg(not(unix))]
        {
            let _ = (name, cb, user_data);
            set_error("virtual ports are not supported by midir on this OS (WinMM); use loopMIDI or Windows MIDI Services loopback endpoints");
            MMIDIR_ERR_UNSUPPORTED
        }
    })
}

/// Closes and frees an input. After return, the callback is never called again. Null-safe.
#[no_mangle]
pub unsafe extern "C" fn mmidir_input_close(handle: *mut InputHandle) -> i32 {
    guard(|| {
        if handle.is_null() {
            return MMIDIR_OK;
        }
        let mut h = Box::from_raw(handle);
        // 1. waits for an in-flight callback, then blocks every later one
        *lock_ignore_poison(&h.alive) = false;
        // 2. drop instead of `close()`: midir's CoreMIDI `close()` takes the handler's
        //    user_data while the port is still connected, and a packet arriving in that
        //    window panics (`user_data.as_mut().unwrap()`) on CoreMIDI's thread, outside
        //    any catch_unwind, aborting the process. Dropping disposes the port/endpoint
        //    with user_data left in place (ALSA/WinMM drop == close).
        drop(h.conn.take());
        MMIDIR_OK
    })
}

// ---------------------------------------------------------------- output

pub struct OutputHandle {
    conn: Option<MidiOutputConnection>,
}

#[no_mangle]
pub unsafe extern "C" fn mmidir_output_open(
    id_ptr: *const u8,
    id_len: usize,
    client_name_ptr: *const u8,
    client_name_len: usize,
    out: *mut *mut OutputHandle,
) -> i32 {
    guard(|| {
        let (Some(id), Some(client)) = (
            str_arg(id_ptr, id_len),
            str_arg(client_name_ptr, client_name_len),
        ) else {
            set_error("invalid argument");
            return MMIDIR_ERR_INVALID_ARG;
        };
        if out.is_null() {
            set_error("out is null");
            return MMIDIR_ERR_INVALID_ARG;
        }
        let mo = match MidiOutput::new(client) {
            Ok(m) => m,
            Err(e) => {
                set_error(e.to_string());
                return MMIDIR_ERR;
            }
        };
        let Some(port) = mo.find_port_by_id(id) else {
            set_error(format!("output port not found: {id}"));
            return MMIDIR_ERR_NOT_FOUND;
        };
        match mo.connect(&port, "midinette-out") {
            Ok(conn) => {
                *out = Box::into_raw(Box::new(OutputHandle { conn: Some(conn) }));
                MMIDIR_OK
            }
            Err(e) => {
                set_error(e.to_string());
                MMIDIR_ERR
            }
        }
    })
}

/// Creates a virtual output (a source other apps can read from). Unix only.
#[no_mangle]
pub unsafe extern "C" fn mmidir_output_create_virtual(
    name_ptr: *const u8,
    name_len: usize,
    out: *mut *mut OutputHandle,
) -> i32 {
    guard(|| {
        let Some(name) = str_arg(name_ptr, name_len) else {
            set_error("invalid argument");
            return MMIDIR_ERR_INVALID_ARG;
        };
        if out.is_null() {
            set_error("out is null");
            return MMIDIR_ERR_INVALID_ARG;
        }
        #[cfg(unix)]
        {
            use midir::os::unix::VirtualOutput;
            let mo = match MidiOutput::new(name) {
                Ok(m) => m,
                Err(e) => {
                    set_error(e.to_string());
                    return MMIDIR_ERR;
                }
            };
            match mo.create_virtual(name) {
                Ok(conn) => {
                    *out = Box::into_raw(Box::new(OutputHandle { conn: Some(conn) }));
                    MMIDIR_OK
                }
                Err(e) => {
                    set_error(e.to_string());
                    MMIDIR_ERR
                }
            }
        }
        #[cfg(not(unix))]
        {
            let _ = name;
            set_error("virtual ports are not supported by midir on this OS (WinMM); use loopMIDI or Windows MIDI Services loopback endpoints");
            MMIDIR_ERR_UNSUPPORTED
        }
    })
}

/// Sends one or more complete MIDI messages (a full F0..F7 sysex is one message).
#[no_mangle]
pub unsafe extern "C" fn mmidir_output_send(
    handle: *mut OutputHandle,
    data: *const u8,
    len: usize,
) -> i32 {
    guard(|| {
        if handle.is_null() || (data.is_null() && len > 0) {
            set_error("invalid argument");
            return MMIDIR_ERR_INVALID_ARG;
        }
        if len == 0 {
            return MMIDIR_OK;
        }
        let h = &mut *handle;
        let Some(conn) = h.conn.as_mut() else {
            set_error("output closed");
            return MMIDIR_ERR;
        };
        let bytes = std::slice::from_raw_parts(data, len);
        match send_chunked(conn, bytes) {
            Ok(()) => MMIDIR_OK,
            Err(e) => {
                set_error(e.to_string());
                MMIDIR_ERR
            }
        }
    })
}

/// CoreMIDI packet framing (midir builds one MIDIPacket per `send`):
/// - MIDIPacket length is a UInt16, a message of 65536 bytes or more is silently dropped:
///   large sysex is sent in 16 KB pieces (CoreMIDI accepts sysex spanning packets).
/// - midir's CoreMIDI *input* treats everything after an F0 up to the packet end as sysex
///   and waits for a packet ending in F7, so a sysex followed by other bytes in the same
///   packet would swallow them: each sysex goes out in its own packet(s).
/// - CoreMIDI (seen on virtual endpoints) truncates a sysex packet at an interleaved
///   realtime byte and drops the data after it: realtime bytes inside a sysex are sent
///   in their own packet just before it (the order a byte-stream parser reports them).
/// - midir's CoreMIDI input stops at a data byte where it expects a status byte (no
///   running status), so running status is expanded to full messages here.
/// Other backends get the bytes unchanged (WinMM requires sysex to start with F0 per call;
/// ALSA's encoder handles any size and running status).
#[cfg(target_os = "macos")]
fn send_chunked(conn: &mut MidiOutputConnection, bytes: &[u8]) -> Result<(), midir::SendError> {
    for packet in coremidi_packets(bytes) {
        conn.send(&packet)?;
    }
    Ok(())
}

/// Splits/normalises a byte stream into CoreMIDI-friendly packets (see `send_chunked`).
/// Pure, so it is unit tested on every OS.
#[cfg_attr(not(target_os = "macos"), allow(dead_code))]
fn coremidi_packets(bytes: &[u8]) -> Vec<Vec<u8>> {
    const CHUNK: usize = 16 * 1024;
    fn data_len(status: u8) -> usize {
        match status {
            0x80..=0xbf | 0xe0..=0xef => 2,
            0xc0..=0xdf => 1,
            0xf1 | 0xf3 => 1,
            0xf2 => 2,
            _ => 0,
        }
    }
    let mut packets: Vec<Vec<u8>> = Vec::new();
    let mut run: Vec<u8> = Vec::new();
    let mut running: u8 = 0;
    let mut pending = 0usize; // data bytes still expected for the current message
    let mut i = 0;
    while i < bytes.len() {
        let b = bytes[i];
        if b == 0xf0 {
            if !run.is_empty() {
                packets.push(std::mem::take(&mut run));
            }
            let end = bytes[i..].iter().position(|&x| x == 0xf7).map(|p| i + p + 1).unwrap_or(bytes.len());
            // realtime bytes interleaved in the sysex are hoisted in front of it
            let (rt, body): (Vec<u8>, Vec<u8>) = bytes[i..end].iter().partition(|&&x| x >= 0xf8);
            if !rt.is_empty() {
                packets.push(rt);
            }
            for piece in body.chunks(CHUNK) {
                packets.push(piece.to_vec());
            }
            running = 0;
            pending = 0;
            i = end;
            continue;
        }
        if b >= 0xf8 {
            run.push(b); // realtime: anywhere, does not touch running status
        } else if b >= 0x80 {
            pending = data_len(b);
            running = if b < 0xf0 { b } else { 0 };
            run.push(b);
        } else if pending > 0 {
            run.push(b);
            pending -= 1;
        } else if running != 0 {
            // running status: re-emit the status byte
            run.push(running);
            run.push(b);
            pending = data_len(running) - 1;
        } else {
            run.push(b); // orphan data byte: pass through unchanged
        }
        i += 1;
    }
    if !run.is_empty() {
        packets.push(run);
    }
    packets
}

#[cfg(test)]
mod tests {
    use super::coremidi_packets;

    #[test]
    fn sysex_gets_its_own_packet() {
        let p = coremidi_packets(&[0x90, 60, 100, 0xf0, 1, 2, 0xf7, 0x80, 60, 0]);
        assert_eq!(p, vec![vec![0x90, 60, 100], vec![0xf0, 1, 2, 0xf7], vec![0x80, 60, 0]]);
    }

    #[test]
    fn running_status_is_expanded() {
        let p = coremidi_packets(&[0x90, 60, 100, 61, 101, 0xf8, 62, 102, 0xc0, 1, 2]);
        assert_eq!(p, vec![vec![0x90, 60, 100, 0x90, 61, 101, 0xf8, 0x90, 62, 102, 0xc0, 1, 0xc0, 2]]);
    }

    #[test]
    fn realtime_inside_sysex_is_hoisted() {
        let p = coremidi_packets(&[0xf0, 0x7d, 1, 0xf8, 2, 0xfe, 3, 0xf7]);
        assert_eq!(p, vec![vec![0xf8, 0xfe], vec![0xf0, 0x7d, 1, 2, 3, 0xf7]]);
    }

    #[test]
    fn big_sysex_is_chunked() {
        let mut s = vec![0xf0u8];
        s.extend(std::iter::repeat(1u8).take(70_000));
        s.push(0xf7);
        let p = coremidi_packets(&s);
        assert!(p.iter().all(|x| x.len() <= 16 * 1024));
        assert_eq!(p.concat(), s);
    }

    #[test]
    fn system_common_cancels_running_status() {
        let p = coremidi_packets(&[0x90, 60, 100, 0xf3, 5, 61]);
        assert_eq!(p, vec![vec![0x90, 60, 100, 0xf3, 5, 61]]);
    }
}

#[cfg(not(target_os = "macos"))]
fn send_chunked(conn: &mut MidiOutputConnection, bytes: &[u8]) -> Result<(), midir::SendError> {
    conn.send(bytes)
}

/// Closes and frees an output. Null-safe.
#[no_mangle]
pub unsafe extern "C" fn mmidir_output_close(handle: *mut OutputHandle) -> i32 {
    guard(|| {
        if handle.is_null() {
            return MMIDIR_OK;
        }
        let mut h = Box::from_raw(handle);
        if let Some(conn) = h.conn.take() {
            let _ = conn.close();
        }
        MMIDIR_OK
    })
}
