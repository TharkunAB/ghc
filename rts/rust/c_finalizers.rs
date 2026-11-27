use core::ffi::c_void;
use core::sync::atomic::{AtomicPtr, Ordering};

core::arch::global_asm!(
    ".hidden ghc_rust_run_c_finalizers",
    ".hidden ghc_rust_run_all_c_finalizers",
);

unsafe extern "C" {
    static stg_NO_FINALIZER_closure: u8;
    static stg_DEAD_WEAK_info: u8;
}

// See Note [Weak finalizer traversal] in Weak.c.
#[repr(C)]
union Callback {
    plain: unsafe extern "C" fn(*mut c_void),
    with_env: unsafe extern "C" fn(*mut c_void, *mut c_void),
}

#[repr(C)]
struct Finalizer {
    link: *mut u8,
    callback: Callback,
    ptr: *mut c_void,
    env: *mut c_void,
    has_env: usize,
}

#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_run_c_finalizers(mut head: *mut u8, payload_offset: usize) {
    while head.cast_const() != &raw const stg_NO_FINALIZER_closure {
        unsafe {
            let node = head.add(payload_offset).cast::<Finalizer>();
            if (*node).has_env != 0 {
                ((*node).callback.with_env)((*node).env, (*node).ptr);
            } else {
                ((*node).callback.plain)((*node).ptr);
            }
            head = (*node).link;
        }
    }
}

#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_run_all_c_finalizers(
    mut weak: *mut u8,
    running: *mut bool,
    callbacks_offset: usize,
    link_offset: usize,
    payload_offset: usize,
) {
    unsafe {
        if !running.is_null() {
            running.write(true);
        }
        while !weak.is_null() {
            let info = AtomicPtr::from_ptr(weak.cast::<*mut u8>()).load(Ordering::Acquire);
            if info.cast_const() != &raw const stg_DEAD_WEAK_info {
                let callbacks = weak.add(callbacks_offset).cast::<*mut u8>().read();
                ghc_rust_run_c_finalizers(callbacks, payload_offset);
            }
            weak = weak.add(link_offset).cast::<*mut u8>().read();
        }
        if !running.is_null() {
            running.write(false);
        }
    }
}
