use core::ffi::c_void;
use core::ptr::{null_mut, write_bytes};

core::arch::global_asm!(".hidden ghc_rust_make_finalizer_batch");

unsafe extern "C" {
    fn allocateMutArrPtrs(
        cap: *mut c_void,
        count: usize,
        init: *mut c_void,
        ccs: *mut c_void,
    ) -> *mut c_void;
    fn mutArrPtrsCardTableSize(count: usize) -> usize;
    static stg_MUT_ARR_PTRS_FROZEN_CLEAN_info: u8;
    static stg_NO_FINALIZER_closure: u8;
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
