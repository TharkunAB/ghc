use core::ffi::c_void;
use core::mem::size_of;

core::arch::global_asm!(
    ".hidden ghc_rust_heap_pap_small",
    ".hidden ghc_rust_heap_pap_large",
);

#[inline(always)]
unsafe fn copy_word(
    out: *mut *mut c_void,
    mut count: usize,
    payload: *const *mut c_void,
    mut bitmap: usize,
    len: usize,
) -> usize {
    unsafe {
        for slot in 0..len {
            if bitmap & 1 == 0 {
                out.add(count).write(payload.add(slot).read());
                count = count.wrapping_add(1);
            }
            bitmap >>= 1;
        }
        count
    }
}

// Note [PAP bitmap traversal]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~
// C decodes the function info and selects the scalar or large bitmap entry.
// Zero bits identify pointer slots. Leave the existing output prefix intact.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_heap_pap_small(
    out: *mut *mut c_void,
    nptrs: *mut usize,
    payload: *const *mut c_void,
    size: usize,
    bitmap: usize,
) {
    unsafe {
        if size == 0 {
            return;
        }
        let initial = nptrs.read();
        let count = copy_word(out, initial, payload, bitmap, size);
        if count != initial {
            nptrs.write(count);
        }
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_heap_pap_large(
    out: *mut *mut c_void,
    nptrs: *mut usize,
    payload: *const *mut c_void,
    size: u32,
    bitmap: *const usize,
) {
    unsafe {
        let size = size as usize;
        if size == 0 {
            return;
        }
        let initial = nptrs.read();
        let mut count = initial;
        let mut offset = 0;
        let word_bits = size_of::<usize>() * 8;
        while offset < size {
            let len = (size - offset).min(word_bits);
            count = copy_word(
                out,
                count,
                payload.add(offset),
                bitmap.add(offset / word_bits).read(),
                len,
            );
            offset += len;
        }
        if count != initial {
            nptrs.write(count);
        }
    }
}
