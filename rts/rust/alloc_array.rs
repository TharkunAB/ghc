use core::ffi::c_void;
use core::mem::size_of;
use core::ptr::null_mut;

core::arch::global_asm!(
    ".hidden ghc_rust_allocate_byte_array",
    ".hidden ghc_rust_allocate_pinned_byte_array",
);

unsafe extern "C" {
    fn allocateMightFail(cap: *mut c_void, words: usize) -> *mut c_void;
    fn allocatePinned(cap: *mut c_void, words: usize, align: usize, offset: usize) -> *mut c_void;
}

// The RTS way determines the header size. C writes the header after allocation.
fn byte_array_words(bytes: usize, header_bytes: usize) -> Option<usize> {
    if bytes > usize::MAX - header_bytes {
        return None;
    }
    Some(header_bytes / size_of::<usize>() + bytes.div_ceil(size_of::<usize>()))
}

#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_allocate_byte_array(
    cap: *mut c_void,
    bytes: usize,
    header_bytes: usize,
) -> *mut c_void {
    match byte_array_words(bytes, header_bytes) {
        Some(words) => unsafe { allocateMightFail(cap, words) },
        None => null_mut(),
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_allocate_pinned_byte_array(
    cap: *mut c_void,
    bytes: usize,
    alignment: usize,
    header_bytes: usize,
) -> *mut c_void {
    match byte_array_words(bytes, header_bytes) {
        Some(words) => unsafe {
            allocatePinned(cap, words, alignment.max(size_of::<usize>()), header_bytes)
        },
        None => null_mut(),
    }
}
