use core::ffi::c_void;

core::arch::global_asm!(".hidden ghc_rust_collect_dead_weaks");

unsafe extern "C" {
    fn mark_root(gc_thread: *mut c_void, slot: *mut *mut u8);
    static stg_NO_FINALIZER_closure: u8;
}

// See Note [Visiting dead weak pointers from Rust] in sm/MarkWeak.c.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_collect_dead_weaks(
    mut weak: *mut u8,
    pending: *mut *mut u8,
    gc_thread: *mut c_void,
    cfinalizers_offset: usize,
    value_offset: usize,
    finalizer_offset: usize,
    link_offset: usize,
) {
    unsafe {
        while !weak.is_null() {
            let cfinalizers = weak.add(cfinalizers_offset).cast::<*const u8>().read();
            if cfinalizers != &raw const stg_NO_FINALIZER_closure {
                mark_root(gc_thread, weak.add(value_offset).cast());
            }
            mark_root(gc_thread, weak.add(finalizer_offset).cast());
            let link = weak.add(link_offset).cast::<*mut u8>();
            let next = link.read();
            link.write(pending.read());
            pending.write(weak);
            weak = next;
        }
    }
}
