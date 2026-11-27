use core::ffi::c_void;
use core::mem::{align_of, size_of};
use core::ptr::null_mut;
use core::sync::atomic::{
    AtomicPtr,
    Ordering::{Relaxed, Release},
};

type Entry = *mut c_void;

const _: () = {
    assert!(size_of::<AtomicPtr<c_void>>() == size_of::<Entry>());
    assert!(align_of::<AtomicPtr<c_void>>() == align_of::<Entry>());
};

// The common object is also linked when C retains its TSAN-instrumented path.
core::arch::global_asm!(
    ".hidden ghc_rust_init_stable_ptr_free_list",
    ".hidden ghc_rust_take_stable_ptr",
    ".hidden ghc_rust_release_stable_ptr",
);

/// Link a new range of table entries in index order. None has a live handle.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_init_stable_ptr_free_list(
    table: *mut Entry,
    count: usize,
) {
    let mut next = null_mut();
    for index in (0..count).rev() {
        let slot = unsafe { table.add(index) };
        unsafe { slot.write(next) };
        next = slot.cast();
    }
}

/// Take the head slot and publish its value for acquire loads in deRefStablePtr.
/// C holds the table mutex and supplies a nonempty free list in this table.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_take_stable_ptr(
    table: *mut Entry,
    head: *mut *mut Entry,
    value: Entry,
) -> usize {
    unsafe {
        let slot = head.read();
        head.write(slot.read().cast());
        AtomicPtr::from_ptr(slot).store(value, Release);
        slot.offset_from_unsigned(table)
    }
}

/// Return a live slot to the free list. C holds the table mutex.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_release_stable_ptr(
    head: *mut *mut Entry,
    slot: *mut Entry,
) {
    unsafe {
        AtomicPtr::from_ptr(slot).store(head.read().cast(), Relaxed);
        head.write(slot);
    }
}

/// Visit live entries in table order. See Note [Stable pointer root slots] in StablePtr.c.
///
/// # Safety
///
/// The caller keeps the table stable during the scan. For a nonzero count,
/// the table contains that many initialized, pointer-sized entries. The
/// caller supplies a valid callback if the table contains live entries.
/// The collector may update the pointer in each slot passed to its callback.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_visit_stable_ptrs(
    table: *mut *mut c_void,
    count: usize,
    visit: Option<unsafe extern "C" fn(*mut c_void, *mut *mut c_void)>,
    user: *mut c_void,
) {
    let bytes = count * size_of::<*mut c_void>();
    for index in 0..count {
        let slot = unsafe { table.add(index) };
        let root = unsafe { slot.read() };
        if !root.is_null() && root.addr().wrapping_sub(table.addr()) >= bytes {
            unsafe { visit.unwrap_unchecked()(user, slot) };
        }
    }
}
