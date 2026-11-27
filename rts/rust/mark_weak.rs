use core::ffi::c_void;

core::arch::global_asm!(".hidden ghc_rust_collect_dead_weaks");

unsafe extern "C" {
    fn mark_root(gc_thread: *mut c_void, slot: *mut *mut u8);
    static stg_NO_FINALIZER_closure: u8;
}

#[repr(C)]
struct WeakPayload {
    cfinalizers: *const u8,
    key: *mut u8,
    value: *mut u8,
    finalizer: *mut u8,
    link: *mut u8,
}

// See Note [Visiting dead weak pointers from Rust] in sm/MarkWeak.c.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_collect_dead_weaks(
    mut weak: *mut u8,
    pending: *mut *mut u8,
    gc_thread: *mut c_void,
    payload_offset: usize,
) {
    unsafe {
        while !weak.is_null() {
            let payload = weak.add(payload_offset).cast::<WeakPayload>();
            if (*payload).cfinalizers != &raw const stg_NO_FINALIZER_closure {
                mark_root(gc_thread, &raw mut (*payload).value);
            }
            mark_root(gc_thread, &raw mut (*payload).finalizer);
            let next = (*payload).link;
            (*payload).link = pending.read();
            pending.write(weak);
            weak = next;
        }
    }
}
