#![no_std]

mod archive_member;
mod arena;
mod encode;
mod exec_page;
mod eventlog_file_writer;
mod file_lock;
mod gc_budget;
mod hash_chain;
mod hash_insert;
mod hash_walk;
#[cfg(ghc_use_large_address_space)]
mod mblock_extents;
mod prim_float;
mod stable_name;
mod stable_ptr;

mod hash_api {
    use core::ffi::{c_int, c_void};

    #[repr(C)]
    pub(super) struct HashTable {
        _private: [u8; 0],
    }

    pub(super) type Hash = unsafe extern "C" fn(*const HashTable, usize) -> c_int;
    type Compare = unsafe extern "C" fn(usize, usize) -> c_int;
    type Free = unsafe extern "C" fn(*mut c_void);

    unsafe extern "C" {
        pub(super) fn allocHashTable() -> *mut HashTable;
        pub(super) fn freeHashTable(table: *mut HashTable, free: Option<Free>);
        pub(super) fn keyCountHashTable(table: *mut HashTable) -> c_int;
        pub(super) fn hashWord(table: *const HashTable, key: usize) -> c_int;
        pub(super) fn insertHashTable(table: *mut HashTable, key: usize, data: *const c_void);
        pub(super) fn removeHashTable(
            table: *mut HashTable,
            key: usize,
            data: *const c_void,
        ) -> *mut c_void;
        pub(super) fn lookupHashTable_(
            table: *const HashTable,
            key: usize,
            hash: Hash,
            compare: Compare,
        ) -> *mut c_void;
        pub(super) fn insertHashTable_(
            table: *mut HashTable,
            key: usize,
            data: *const c_void,
            hash: Hash,
        );
        pub(super) fn removeHashTable_(
            table: *mut HashTable,
            key: usize,
            data: *const c_void,
            hash: Hash,
            compare: Compare,
        ) -> *mut c_void;
    }
}
