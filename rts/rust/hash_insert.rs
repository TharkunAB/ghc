use core::{
    ffi::{c_int, c_void},
    mem::size_of,
    ptr,
};

use super::hash_walk::{Entry, SEGMENT_SIZE};

const DIRECTORY_SIZE: usize = 1024;
const LOAD: c_int = 5;

// Hash.c checks this prefix layout and the constants used for expansion.
#[repr(C)]
pub(super) struct Table {
    split: c_int,
    max: c_int,
    mask1: c_int,
    mask2: c_int,
    key_count: c_int,
    bucket_count: c_int,
    directory: [*mut *mut Entry; DIRECTORY_SIZE],
    free_list: *mut Entry,
}

type Hash = unsafe extern "C" fn(*const Table, usize) -> c_int;

unsafe extern "C" {
    fn ghc_c_hash_alloc_segment(table: *mut Table, segment: c_int);
    fn ghc_c_hash_alloc_entry(table: *mut Table) -> *mut Entry;
}

unsafe fn bucket_slot(table: *mut Table, index: usize) -> *mut *mut Entry {
    let directory = unsafe { (&raw mut (*table).directory).cast::<*mut *mut Entry>() };
    unsafe {
        directory
            .add(index / SEGMENT_SIZE)
            .read()
            .add(index % SEGMENT_SIZE)
    }
}

unsafe fn expand(table: *mut Table, hash: impl Fn(*const Table, usize) -> c_int) {
    unsafe {
        let old_bucket = (*table).split as usize;
        let new_bucket = old_bucket + (*table).max as usize;
        if new_bucket == DIRECTORY_SIZE * SEGMENT_SIZE {
            return;
        }

        if new_bucket.is_multiple_of(SEGMENT_SIZE) {
            ghc_c_hash_alloc_segment(table, (new_bucket / SEGMENT_SIZE) as c_int);
        }
        let old_slot = bucket_slot(table, old_bucket);
        let new_slot = bucket_slot(table, new_bucket);

        // Publish the initialized bucket and new limits before hashing any
        // entry. A custom hash function can inspect the table during the split.
        new_slot.write(ptr::null_mut());
        (*table).split += 1;
        if (*table).split == (*table).max {
            (*table).split = 0;
            (*table).max *= 2;
            (*table).mask1 = (*table).mask2;
            (*table).mask2 = ((*table).mask2 << 1) | 1;
        }
        (*table).bucket_count += 1;

        // Visit the old chain in order. Prepend to each output chain, as in C.
        // Save the next pointer before the callback and publish both heads
        // only after the walk.
        let mut old_head = ptr::null_mut();
        let mut new_head = ptr::null_mut();
        let mut entry = old_slot.read();
        while !entry.is_null() {
            let next = (*entry).next;
            if hash(table, (*entry).key) as usize == new_bucket {
                (*entry).next = new_head;
                new_head = entry;
            } else {
                (*entry).next = old_head;
                old_head = entry;
            }
            entry = next;
        }
        old_slot.write(old_head);
        new_slot.write(new_head);
    }
}

unsafe fn insert(
    table: *mut Table,
    key: usize,
    value: *const c_void,
    hash: impl Fn(*const Table, usize) -> c_int + Copy,
) {
    unsafe {
        (*table).key_count += 1;
        if (*table).key_count >= LOAD * (*table).bucket_count {
            expand(table, hash);
        }
        let slot = bucket_slot(table, hash(table, key) as usize);

        let mut entry = (*table).free_list;
        if entry.is_null() {
            entry = ghc_c_hash_alloc_entry(table);
        } else {
            (*table).free_list = (*entry).next;
        }
        entry.write(Entry {
            key,
            value,
            next: slot.read(),
        });
        slot.write(entry);
    }
}

/// Insert a word key, splitting a bucket when the load reaches the limit.
///
/// # Safety
/// The table must be initialized and the caller must exclude concurrent or
/// reentrant mutation.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_hash_insert_word(
    table: *mut Table,
    key: usize,
    value: *const c_void,
) {
    unsafe {
        insert(table, key, value, |table, key| {
            let word = key / size_of::<usize>();
            let bucket = (word & (*table).mask1 as usize) as c_int;
            if bucket < (*table).split {
                (word & (*table).mask2 as usize) as c_int
            } else {
                bucket
            }
        });
    }
}

/// Insert a key using a C hash function.
///
/// # Safety
/// The table must be initialized. The caller must exclude concurrent and
/// reentrant mutation. The callback must accept all keys in the affected
/// bucket and return an initialized bucket index. It may inspect the table.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_hash_insert_custom(
    table: *mut Table,
    key: usize,
    value: *const c_void,
    hash: Hash,
) {
    unsafe { insert(table, key, value, |table, key| hash(table, key)) }
}
