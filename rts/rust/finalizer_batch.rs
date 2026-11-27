use core::ffi::c_void;
use core::mem::{align_of, size_of};
use core::ptr::{null_mut, write_bytes};
use core::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};

core::arch::global_asm!(
    ".hidden ghc_rust_make_finalizer_batch",
    ".hidden ghc_rust_prepare_finalizers",
);

const _: () = {
    assert!(size_of::<AtomicPtr<u8>>() == size_of::<*mut u8>());
    assert!(align_of::<AtomicPtr<u8>>() == align_of::<*mut u8>());
    assert!(size_of::<AtomicUsize>() == size_of::<usize>());
    assert!(align_of::<AtomicUsize>() == align_of::<usize>());
};

unsafe extern "C" {
    fn allocateMutArrPtrs(
        cap: *mut c_void,
        count: usize,
        init: *mut c_void,
        ccs: *mut c_void,
    ) -> *mut c_void;
    fn mutArrPtrsCardTableSize(count: usize) -> usize;
    static stg_MUT_ARR_PTRS_FROZEN_CLEAN_info: u8;
    static stg_DEAD_WEAK_info: u8;
    static stg_NO_FINALIZER_closure: u8;
}

/// Append dead weak pointers, mark the new entries, and publish their count.
/// The GC owns `list`. The idle drain can still hold entries in `head`.
/// C supplies the weak link offset and any DEBUG or profiling header action.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_prepare_finalizers(
    head: *mut *mut u8,
    list: *mut u8,
    pending: *mut usize,
    link_offset: usize,
    prepare_header: Option<unsafe extern "C" fn(*mut u8)>,
) -> usize {
    unsafe {
        let mut tail = head;
        loop {
            let weak = tail.read();
            if weak.is_null() {
                break;
            }
            tail = weak.byte_add(link_offset).cast::<*mut u8>();
        }
        AtomicPtr::from_ptr(tail).store(list, Ordering::SeqCst);

        let mut actions = 0;
        let mut entries = 0;
        let mut weak = list;
        while !weak.is_null() {
            if let Some(prepare_header) = prepare_header {
                prepare_header(weak);
            }
            let finalizer = weak
                .byte_add(link_offset - size_of::<usize>())
                .cast::<*mut u8>()
                .read();
            if finalizer.cast_const() != &raw const stg_NO_FINALIZER_closure {
                actions += 1;
            }
            entries += 1;
            AtomicPtr::from_ptr(weak.cast::<*mut u8>()).store(
                (&raw const stg_DEAD_WEAK_info).cast_mut(),
                Ordering::Relaxed,
            );
            weak = weak.byte_add(link_offset).cast::<*mut u8>().read();
        }
        AtomicUsize::from_ptr(pending).fetch_add(entries, Ordering::SeqCst);
        actions
    }
}

// The GC has counted the Haskell finalizers in this list and still owns it.
// runFinalizerBatch reads the completed array from the last slot to the first.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_make_finalizer_batch(
    cap: *mut c_void,
    mut weak: *mut u8,
    count: usize,
    ccs: *mut c_void,
    finalizer_offset: usize,
    link_offset: usize,
    payload_offset: usize,
) -> *mut c_void {
    unsafe {
        let array = allocateMutArrPtrs(cap, count, null_mut(), ccs);
        if array.is_null() {
            return array;
        }
        array
            .cast::<*const u8>()
            .write(&raw const stg_MUT_ARR_PTRS_FROZEN_CLEAN_info);
        let mut slot = array.byte_add(payload_offset).cast::<*mut u8>();
        while !weak.is_null() {
            let finalizer = weak.add(finalizer_offset).cast::<*mut u8>().read();
            if finalizer.cast_const() != &raw const stg_NO_FINALIZER_closure {
                slot.write(finalizer);
                slot = slot.add(1);
            }
            weak = weak.add(link_offset).cast::<*mut u8>().read();
        }
        write_bytes(slot.cast::<usize>(), 0xff, mutArrPtrsCardTableSize(count));
        array
    }
}
