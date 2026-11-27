use core::ffi::c_void;
use core::mem::size_of;
use core::ptr::null_mut;

core::arch::global_asm!(
    ".hidden ghc_rust_allocate_mut_array",
    ".hidden ghc_rust_allocate_small_mut_array",
    ".hidden ghc_rust_allocate_byte_array",
    ".hidden ghc_rust_allocate_pinned_byte_array",
);

unsafe extern "C" {
    static stg_MUT_ARR_PTRS_DIRTY_info: u8;
    static stg_SMALL_MUT_ARR_PTRS_DIRTY_info: u8;
    fn mutArrPtrsCards(count: usize) -> usize;
    fn ghc_rust_set_array_header(
        array: *mut c_void,
        info: *const u8,
        ccs: *mut c_void,
        header_words: usize,
        payload_words: usize,
    );
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

/// Allocate a pointer array. Only the ordinary array has a card table.
unsafe fn allocate_pointer_array<const SMALL: bool>(
    cap: *mut c_void,
    count: usize,
    init: *mut c_void,
    ccs: *mut c_void,
    header_bytes: usize,
) -> *mut c_void {
    let word = size_of::<usize>();
    if count > (usize::MAX - header_bytes) / word {
        return null_mut();
    }
    let card_bytes = if SMALL {
        0
    } else {
        unsafe { mutArrPtrsCards(count) }
    };
    let payload_words = count + card_bytes.div_ceil(word);
    let header_words = header_bytes / word;
    unsafe {
        let array = allocateMightFail(cap, header_words + payload_words);
        if array.is_null() {
            return null_mut();
        }
        let slots = array.byte_add(header_bytes).cast::<*mut c_void>();
        let metadata_end = slots.cast::<usize>();
        if SMALL {
            ghc_rust_set_array_header(
                array,
                &raw const stg_SMALL_MUT_ARR_PTRS_DIRTY_info,
                ccs,
                header_words,
                payload_words,
            );
            metadata_end.sub(1).write(count);
        } else {
            ghc_rust_set_array_header(
                array,
                &raw const stg_MUT_ARR_PTRS_DIRTY_info,
                ccs,
                header_words,
                payload_words,
            );
            metadata_end.sub(2).write(count);
            metadata_end.sub(1).write(payload_words);
        }
        if !init.is_null() {
            for index in 0..count {
                slots.add(index).write(init);
            }
        }
        if !SMALL {
            // The trailing alignment bytes of the card table stay untouched.
            slots.add(count).cast::<u8>().write_bytes(0, card_bytes);
        }
        array
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_allocate_mut_array(
    cap: *mut c_void,
    count: usize,
    init: *mut c_void,
    ccs: *mut c_void,
    header_bytes: usize,
) -> *mut c_void {
    unsafe { allocate_pointer_array::<false>(cap, count, init, ccs, header_bytes) }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_allocate_small_mut_array(
    cap: *mut c_void,
    count: usize,
    init: *mut c_void,
    ccs: *mut c_void,
    header_bytes: usize,
) -> *mut c_void {
    unsafe { allocate_pointer_array::<true>(cap, count, init, ccs, header_bytes) }
}
