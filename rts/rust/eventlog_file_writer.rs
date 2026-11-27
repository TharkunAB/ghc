use core::{
    ffi::{c_int, c_void},
    ptr,
    sync::atomic::{AtomicPtr, Ordering::Relaxed},
};

unsafe extern "C" {
    fn ghc_rust_eventlog_open() -> *mut c_void;
    fn ghc_rust_eventlog_acquire();
    fn ghc_rust_eventlog_release();
    fn ghc_rust_eventlog_close_mutex();

    fn fwrite(data: *const u8, size: usize, count: usize, file: *mut c_void) -> usize;
    fn fflush(file: *mut c_void) -> c_int;
    fn fclose(file: *mut c_void) -> c_int;
}

// EventLog.c serializes init and stop with the writer callbacks. The RTS
// mutex protects the whole write loop. libc serializes flushes with writes.
static EVENT_LOG_FILE: AtomicPtr<c_void> = AtomicPtr::new(ptr::null_mut());

/// Open the file and initialize the writer mutex.
///
/// # Safety
///
/// No other writer callback may run during initialization.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_eventlog_init() {
    EVENT_LOG_FILE.store(unsafe { ghc_rust_eventlog_open() }, Relaxed);
}

/// Write all bytes, or return false when a write reports no progress.
///
/// # Safety
///
/// The writer must be initialized. The buffer must be readable for
/// `remaining` bytes for the duration of the call.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_eventlog_write(
    mut data: *const u8,
    mut remaining: usize,
) -> bool {
    unsafe { ghc_rust_eventlog_acquire() };
    let file = EVENT_LOG_FILE.load(Relaxed);
    while remaining > 0 {
        let written = unsafe { fwrite(data, 1, remaining, file) };
        if written == 0 {
            unsafe { ghc_rust_eventlog_release() };
            return false;
        }
        remaining -= written;
        data = unsafe { data.add(written) };
    }
    unsafe { ghc_rust_eventlog_release() };

    unsafe { ghc_rust_eventlog_flush() };
    true
}

/// Flush the file if it is open.
///
/// # Safety
///
/// Initialization and shutdown must be serialized with this call.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_eventlog_flush() {
    let file = EVENT_LOG_FILE.load(Relaxed);
    if !file.is_null() {
        unsafe { fflush(file) };
    }
}

/// Close the file and destroy the writer mutex.
///
/// # Safety
///
/// The writer must be initialized. No other writer callback may run during
/// shutdown.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_eventlog_stop() {
    let file = EVENT_LOG_FILE.load(Relaxed);
    if !file.is_null() {
        unsafe { fclose(file) };
        EVENT_LOG_FILE.store(ptr::null_mut(), Relaxed);
    }
    unsafe { ghc_rust_eventlog_close_mutex() };
}
