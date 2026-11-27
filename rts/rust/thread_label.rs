use core::ffi::c_void;
use core::mem::{align_of, size_of};
use core::sync::atomic::{AtomicPtr, Ordering};

const _: () = {
    assert!(size_of::<AtomicPtr<c_void>>() == size_of::<*mut c_void>());
    assert!(align_of::<AtomicPtr<c_void>>() == align_of::<*mut c_void>());
};

core::arch::global_asm!(".hidden ghc_rust_label_thread");

unsafe extern "C" {
    fn ghc_rust_thread_label_before_store(
        cap: *mut c_void,
        tso: *mut c_void,
        old_label: *mut c_void,
    );
}

/// Publish a thread label after C has recorded the old label for the collector.
/// C owns the TSO layout and emits the event after this call returns.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_label_thread(
    slot: *mut *mut c_void,
    cap: *mut c_void,
    tso: *mut c_void,
    label: *mut c_void,
) {
    unsafe {
        let slot = AtomicPtr::from_ptr(slot);
        let old_label = slot.load(Ordering::Acquire);
        ghc_rust_thread_label_before_store(cap, tso, old_label);
        slot.store(label, Ordering::Release);
    }
}
