use core::ffi::{c_char, c_int, c_void};
use core::mem::{offset_of, size_of};

core::arch::global_asm!(
    ".hidden ghc_rust_foreign_exports_enqueue",
    ".hidden ghc_rust_foreign_exports_process",
);

type StablePtr = *mut c_void;

/// Match the list that generated C constructors register with the RTS.
#[repr(C)]
pub(super) struct ForeignExportsList {
    next: *mut ForeignExportsList,
    n_entries: c_int,
    oc: *mut c_void,
    stable_ptrs: *mut *mut StablePtr,
    exports: [*mut c_void; 0],
}

const _: () = {
    assert!(size_of::<ForeignExportsList>() == 32);
    assert!(offset_of!(ForeignExportsList, next) == 0);
    assert!(offset_of!(ForeignExportsList, n_entries) == 8);
    assert!(offset_of!(ForeignExportsList, oc) == 16);
    assert!(offset_of!(ForeignExportsList, stable_ptrs) == 24);
    assert!(offset_of!(ForeignExportsList, exports) == 32);
};

unsafe extern "C" {
    fn stgMallocBytes(size: usize, label: *const c_char) -> *mut c_void;
    fn getStablePtr(closure: *mut c_void) -> StablePtr;
}

/// Queue exports without allocation. Constructors can call this before RTS startup.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_foreign_exports_enqueue(
    pending: *mut *mut ForeignExportsList,
    loading_obj: *mut c_void,
    exports: *mut ForeignExportsList,
) {
    unsafe {
        (*exports).next = *pending;
        (*exports).oc = loading_obj;
        *pending = exports;
    }
}

/// Create roots after the stable pointer table is ready.
/// The caller holds the linker lock after RTS startup.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_foreign_exports_process(
    pending: *mut *mut ForeignExportsList,
    check: Option<unsafe extern "C" fn(*mut ForeignExportsList)>,
    head: unsafe extern "C" fn(*mut c_void) -> *mut *mut ForeignExportsList,
) {
    unsafe {
        while !(*pending).is_null() {
            let exports = *pending;
            *pending = (*exports).next;
            if let Some(check) = check {
                check(exports);
            }

            let count = (*exports).n_entries;
            let closures = (&raw const (*exports).exports).cast::<*mut c_void>();
            if !(*exports).oc.is_null() {
                let bytes = size_of::<*mut StablePtr>().wrapping_mul(count as usize);
                let roots = stgMallocBytes(bytes, c"foreignExportStablePtr".as_ptr())
                    .cast::<*mut StablePtr>();
                (*exports).stable_ptrs = roots;
                for i in 0..count {
                    let i = i as usize;
                    roots
                        .add(i)
                        .write(getStablePtr(closures.add(i).read()).cast::<StablePtr>());
                }

                let owner_head = head((*exports).oc);
                (*exports).next = *owner_head;
                *owner_head = exports;
            } else {
                for i in 0..count {
                    let i = i as usize;
                    getStablePtr(closures.add(i).read());
                }
            }
        }
    }
}
