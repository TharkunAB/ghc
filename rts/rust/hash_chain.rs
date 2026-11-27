use core::{
    ffi::{c_char, c_int, c_void},
    ptr,
};

use super::hash_walk::Entry;

type Compare = unsafe extern "C" fn(usize, usize) -> c_int;

unsafe extern "C" {
    fn strcmp(left: *const c_char, right: *const c_char) -> c_int;
}

unsafe fn lookup(
    mut entry: *mut Entry,
    key: usize,
    equal: impl Fn(usize, usize) -> bool,
) -> *const c_void {
    while !entry.is_null() {
        if equal(unsafe { (*entry).key }, key) {
            return unsafe { (*entry).value };
        }
        entry = unsafe { (*entry).next };
    }
    ptr::null()
}

unsafe fn unlink(
    mut link: *mut *mut Entry,
    key: usize,
    value: *const c_void,
    equal: impl Fn(usize, usize) -> bool,
) -> *mut Entry {
    loop {
        let entry = unsafe { link.read() };
        if entry.is_null() {
            return entry;
        }
        if equal(unsafe { (*entry).key }, key)
            && (value.is_null() || unsafe { (*entry).value == value })
        {
            unsafe { link.write((*entry).next) };
            // C owns the detached node and returns it to the freelist.
            return entry;
        }
        link = unsafe { &raw mut (*entry).next };
    }
}

/// Find the value of the first matching word key.
///
/// # Safety
///
/// The bucket chain must remain valid and stable during traversal.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_hash_lookup_word(
    head: *mut Entry,
    key: usize,
) -> *const c_void {
    unsafe { lookup(head, key, |left, right| left == right) }
}

/// Find the value of the first matching C string.
///
/// # Safety
///
/// The bucket chain must remain stable and every compared key must point to
/// a valid NUL-terminated string.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_hash_lookup_str(
    head: *mut Entry,
    key: usize,
) -> *const c_void {
    unsafe {
        lookup(head, key, |left, right| {
            strcmp(left as *const c_char, right as *const c_char) == 0
        })
    }
}

/// Find the value of the first matching key according to the C callback.
///
/// # Safety
///
/// The bucket chain must remain valid during traversal. A nonempty chain
/// requires a valid callback, which may inspect the table but must not
/// modify the chain.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_hash_lookup_custom(
    head: *mut Entry,
    key: usize,
    compare: Option<Compare>,
) -> *const c_void {
    unsafe {
        lookup(head, key, |left, right| {
            compare.unwrap_unchecked()(left, right) != 0
        })
    }
}

/// Detach the first matching word key and optional value.
///
/// # Safety
///
/// The bucket head slot and chain must be valid, with exclusive access.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_hash_unlink_word(
    head: *mut *mut Entry,
    key: usize,
    value: *const c_void,
) -> *mut Entry {
    unsafe { unlink(head, key, value, |left, right| left == right) }
}

/// Detach the first matching C string and optional value.
///
/// # Safety
///
/// The bucket head slot and chain require exclusive access. All compared
/// keys must point to valid NUL-terminated strings.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_hash_unlink_str(
    head: *mut *mut Entry,
    key: usize,
    value: *const c_void,
) -> *mut Entry {
    unsafe {
        unlink(head, key, value, |left, right| {
            strcmp(left as *const c_char, right as *const c_char) == 0
        })
    }
}

/// Detach the first matching callback key and optional value.
///
/// # Safety
///
/// The bucket head and chain must be valid, with exclusive write access.
/// A nonempty chain requires a valid callback that must not modify the chain.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_hash_unlink_custom(
    head: *mut *mut Entry,
    key: usize,
    value: *const c_void,
    compare: Option<Compare>,
) -> *mut Entry {
    unsafe {
        unlink(head, key, value, |left, right| {
            compare.unwrap_unchecked()(left, right) != 0
        })
    }
}
