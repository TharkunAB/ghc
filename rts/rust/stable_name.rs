use core::{ffi::c_void, mem::size_of};

#[repr(C)]
pub(super) struct Entry {
    addr: *mut c_void,
    old: *mut c_void,
    sn_obj: *mut c_void,
}

// See Note [Stable name table visits] in StableName.h. Callers serialize
// changes to the table and allow the collector to rewrite individual slots.
unsafe fn visit_entries(table: *mut Entry, count: usize, mut visit: impl FnMut(*mut Entry)) {
    let bytes = count * size_of::<Entry>();
    for index in 1..count {
        let entry = unsafe { table.add(index) };
        let addr = unsafe { (*entry).addr };
        if addr.addr().wrapping_sub(table.addr()) >= bytes {
            visit(entry);
        }
    }
}

/// Save each visited address before the collector moves the pointee.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_remember_stable_names(table: *mut Entry, count: usize) {
    unsafe { visit_entries(table, count, |entry| (*entry).old = (*entry).addr) };
}

/// Give the collector each non-null name slot, then its non-null pointee slot.
/// The collector can update the slots in place.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_thread_stable_names(
    table: *mut Entry,
    count: usize,
    evac: Option<unsafe extern "C" fn(*mut c_void, *mut *mut c_void)>,
    user: *mut c_void,
) {
    unsafe {
        visit_entries(table, count, |entry| {
            if !(*entry).sn_obj.is_null() {
                evac.unwrap_unchecked()(user, &raw mut (*entry).sn_obj);
            }
            if !(*entry).addr.is_null() {
                evac.unwrap_unchecked()(user, &raw mut (*entry).addr);
            }
        });
    }
}
