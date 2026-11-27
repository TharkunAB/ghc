use core::ffi::c_void;
use core::ptr::null_mut;
use core::sync::atomic::{AtomicPtr, Ordering};

core::arch::global_asm!(
    ".hidden ghc_rust_collect_dead_weaks",
    ".hidden ghc_rust_mark_weak_list",
    ".hidden ghc_rust_collect_fresh_weaks",
);

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

#[repr(C)]
struct WeakList {
    head: *mut u8,
    tail: *mut u8,
}

// Each capability contributes one list, in ascending capability order.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_collect_fresh_weaks(
    nursery: *mut *mut u8,
    caps: *mut *mut u8,
    count: u32,
    endpoints_offset: usize,
    payload_offset: usize,
    check: Option<unsafe extern "C" fn(*mut u8, *mut u8)>,
) {
    unsafe {
        for i in 0..count as usize {
            let cap = AtomicPtr::from_ptr(caps.add(i)).load(Ordering::Relaxed);
            let list = cap.add(endpoints_offset).cast::<WeakList>();
            let head = (*list).head;
            let tail = (*list).tail;
            if let Some(check) = check {
                check(head, tail);
            }
            if !tail.is_null() {
                let payload = tail.add(payload_offset).cast::<WeakPayload>();
                (*payload).link = nursery.read();
                nursery.write(head);
                (*list).tail = null_mut();
                (*list).head = null_mut();
            }
        }
    }
}

// Evacuation can replace the object in the root slot. Follow the link in its new location.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_mark_weak_list(
    mut slot: *mut *mut u8,
    gc_thread: *mut c_void,
    payload_offset: usize,
    check: Option<unsafe extern "C" fn(*mut u8)>,
) {
    unsafe {
        loop {
            let weak = AtomicPtr::from_ptr(slot).load(Ordering::Relaxed);
            if weak.is_null() {
                return;
            }
            if let Some(check) = check {
                check(weak);
            }
            mark_root(gc_thread, slot);
            let moved = slot.read().add(payload_offset).cast::<WeakPayload>();
            slot = &raw mut (*moved).link;
        }
    }
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
