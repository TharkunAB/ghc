#![no_std]

use core::ffi::c_void;
use core::mem::align_of;
use core::ptr::null_mut;
use core::sync::atomic::{AtomicIsize, AtomicPtr, Ordering::*, fence};

#[repr(C)]
pub struct WSDeque {
    size: isize,
    modulo_size: usize,
    top: isize,
    bottom: isize,
    elements: *mut *mut c_void,
}

const _: () = {
    assert!(align_of::<AtomicIsize>() == align_of::<isize>());
    assert!(align_of::<AtomicPtr<c_void>>() == align_of::<*mut c_void>());
};

// Note [Concurrent deque storage]
// -------------------------------
// C allocates and frees the deque. C layout checks cover this x86-64 mirror
// and the atomic alignment. Concurrent operations access indices and slots
// atomically at their original widths. The size, mask and array pointer are
// immutable after publication. Each atomic reference covers one field or slot.
// The caller excludes concurrent operations during collection and destruction.
// The array size is a positive power of two. Live counters are nonnegative
// and do not overflow. One owner may push or pop, and multiple thieves
// may steal.

// User-supplied C compiler flags can enable TSAN without the Hadrian flavour.
// In that case C omits its references. Keep these ELF symbols private even
// if the Rust object is still selected in a shared RTS.
core::arch::global_asm!(
    ".hidden ghc_rust_push_wsdeque",
    ".hidden ghc_rust_pop_wsdeque",
    ".hidden ghc_rust_steal_wsdeque_once",
    ".hidden ghc_rust_steal_wsdeque",
);

#[inline(always)]
unsafe fn slot(q: *mut WSDeque, index: usize) -> *mut *mut c_void {
    unsafe { (*q).elements.add(index) }
}

/// # Safety
/// The deque must be live and initialized as described in Note [Concurrent
/// deque storage]. Only the owner may call this function.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ghc_rust_push_wsdeque(q: *mut WSDeque, elem: *mut c_void) -> bool {
    unsafe {
        let bottom = AtomicIsize::from_ptr(&raw mut (*q).bottom);
        let b = bottom.load(Acquire);
        let t = AtomicIsize::from_ptr(&raw mut (*q).top).load(Acquire);
        if b.wrapping_sub(t) > (*q).size - 1 {
            return false;
        }

        AtomicPtr::from_ptr(slot(q, b as usize & (*q).modulo_size)).store(elem, Relaxed);
        fence(Release);
        bottom.store(b.wrapping_add(1), Relaxed);
        true
    }
}

/// # Safety
/// The deque must be live and initialized as described in Note [Concurrent
/// deque storage]. Only the owner may call this function.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ghc_rust_pop_wsdeque(q: *mut WSDeque) -> *mut c_void {
    unsafe {
        let bottom = AtomicIsize::from_ptr(&raw mut (*q).bottom);
        let b = bottom.load(Relaxed).wrapping_sub(1);
        bottom.store(b, Relaxed);
        fence(SeqCst);
        let top = AtomicIsize::from_ptr(&raw mut (*q).top);
        let t = top.load(Relaxed);

        let mut result = null_mut();
        if t <= b {
            result = AtomicPtr::from_ptr(slot(q, b as usize & (*q).modulo_size)).load(Relaxed);
            if t < b {
                return result;
            }
            if top
                .compare_exchange(t, t.wrapping_add(1), SeqCst, Relaxed)
                .is_err()
            {
                result = null_mut();
            }
        }
        bottom.store(b.wrapping_add(1), Relaxed);
        result
    }
}

#[inline(always)]
unsafe fn steal_once(q: *mut WSDeque) -> *mut c_void {
    unsafe {
        let top = AtomicIsize::from_ptr(&raw mut (*q).top);
        let t = top.load(Acquire);
        fence(SeqCst);
        let b = AtomicIsize::from_ptr(&raw mut (*q).bottom).load(Acquire);
        if t >= b {
            return null_mut();
        }

        // A live top is nonnegative and size is a power of two.
        let result = AtomicPtr::from_ptr(slot(q, t as usize & (*q).modulo_size)).load(Relaxed);
        if top
            .compare_exchange(t, t.wrapping_add(1), SeqCst, Relaxed)
            .is_err()
        {
            return null_mut();
        }
        result
    }
}

/// # Safety
/// The deque must be live and initialized as described in Note [Concurrent
/// deque storage]. Thieves may call this function concurrently.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ghc_rust_steal_wsdeque_once(q: *mut WSDeque) -> *mut c_void {
    unsafe { steal_once(q) }
}

/// # Safety
/// The deque must be live and initialized as described in Note [Concurrent
/// deque storage]. Thieves may call this function concurrently.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn ghc_rust_steal_wsdeque(q: *mut WSDeque) -> *mut c_void {
    unsafe {
        loop {
            let result = steal_once(q);
            if !result.is_null() {
                return result;
            }

            // Match looksEmptyWSDeque. Load top before bottom.
            let t = AtomicIsize::from_ptr(&raw mut (*q).top).load(Acquire);
            let b = AtomicIsize::from_ptr(&raw mut (*q).bottom).load(Acquire);
            if b.wrapping_sub(t) <= 0 {
                return null_mut();
            }
        }
    }
}
