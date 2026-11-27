use core::{ffi::c_void, mem::size_of};

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
