use core::{ffi::c_void, mem::size_of, ptr};

use super::hash_api::{
    HashTable, allocHashTable, freeHashTable, insertHashTable, keyCountHashTable, removeHashTable,
};

core::arch::global_asm!(".hidden ghc_rust_reserve_stable_name");

#[repr(C)]
pub(super) struct Entry {
    addr: *mut c_void,
    old: *mut c_void,
    sn_obj: *mut c_void,
}

/// Take one free slot and add its address to the index.
/// C holds the table lock and publishes the name object after this call.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_reserve_stable_name(
    table: *mut Entry,
    head: *mut *mut Entry,
    hash: *mut HashTable,
    addr: *mut c_void,
) -> usize {
    unsafe {
        let slot = head.read();
        head.write((*slot).addr.cast());
        (*slot).addr = addr;
        (*slot).sn_obj = ptr::null_mut();
        let index = slot.offset_from_unsigned(table);
        insertHashTable(hash, addr.addr(), ptr::with_exposed_provenance(index));
        index
    }
}

// See Note [Stable name table visits] in StableName.h. Callers serialize
// changes to the table and allow the collector to rewrite individual slots.
unsafe fn visit_entries(table: *mut Entry, count: usize, mut visit: impl FnMut(usize, *mut Entry)) {
    let bytes = count * size_of::<Entry>();
    for index in 1..count {
        let entry = unsafe { table.add(index) };
        let addr = unsafe { (*entry).addr };
        if addr.addr().wrapping_sub(table.addr()) >= bytes {
            visit(index, entry);
        }
    }
}

/// Save each visited address before the collector moves the pointee.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_remember_stable_names(table: *mut Entry, count: usize) {
    unsafe { visit_entries(table, count, |_, entry| (*entry).old = (*entry).addr) };
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
        visit_entries(table, count, |_, entry| {
            if !(*entry).sn_obj.is_null() {
                evac.unwrap_unchecked()(user, &raw mut (*entry).sn_obj);
            }
            if !(*entry).addr.is_null() {
                evac.unwrap_unchecked()(user, &raw mut (*entry).addr);
            }
        });
    }
}

/// Update the address index after the collector visits stable names.
/// C serializes access to the entries and the hash table.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_update_stable_names(
    table: *mut Entry,
    count: usize,
    hash: *mut *mut HashTable,
    full: bool,
) {
    unsafe {
        let mut index_by_addr = hash.read();
        if full && !index_by_addr.is_null() && keyCountHashTable(index_by_addr) != 0 {
            freeHashTable(index_by_addr, None);
            index_by_addr = allocHashTable();
            hash.write(index_by_addr);
        }

        visit_entries(table, count, |index, entry| {
            let addr = (*entry).addr;
            if !full {
                let old = (*entry).old;
                if addr == old {
                    return;
                }
                removeHashTable(index_by_addr, old.addr(), ptr::null());
            }
            if !addr.is_null() {
                insertHashTable(
                    index_by_addr,
                    addr.addr(),
                    ptr::with_exposed_provenance(index),
                );
            }
        });
    }
}

/// Return a dead name entry to the free list.
/// C has determined that the name object died and holds the table lock.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_free_stable_name(
    entry: *mut Entry,
    head: *mut *mut Entry,
    hash: *mut HashTable,
) {
    unsafe {
        removeHashTable(hash, (*entry).old.addr(), ptr::null());
        (*entry).addr = head.read().cast();
        head.write(entry);
    }
}
