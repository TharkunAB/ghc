use core::ffi::{c_int, c_void};
use core::ptr::{copy_nonoverlapping, write_bytes};

core::arch::global_asm!(".hidden ghc_rust_closure_table_init_growth");

/// Copy the old closure table data and initialize the new entries.
///
/// # Safety
///
/// C owns the arrays and has exclusive access to them. The old and new
/// allocations do not overlap. Their payloads hold the given numbers of
/// pointer slots, card words, and free-list words. Both old pointers may be
/// null only when the old capacity is zero. The new capacity is greater than
/// the old capacity and fits in `c_int`. C checks the word layout.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_closure_table_init_growth(
    old_payload: *const *mut c_void,
    new_payload: *mut *mut c_void,
    old_free: *const usize,
    new_free: *mut usize,
    old_capacity: usize,
    new_capacity: usize,
    old_card_words: usize,
    new_card_words: usize,
    old_free_words: usize,
    old_free_head: c_int,
    null_closure: *mut c_void,
) {
    if old_capacity != 0 {
        unsafe {
            copy_nonoverlapping(old_payload, new_payload, old_capacity);
            copy_nonoverlapping(old_free, new_free, old_free_words);
        }
    }
    for index in old_capacity..new_capacity {
        unsafe { new_payload.add(index).write(null_closure) };
    }
    if old_card_words != 0 {
        unsafe {
            copy_nonoverlapping(
                old_payload.add(old_capacity),
                new_payload.add(new_capacity),
                old_card_words,
            );
        }
    }
    unsafe {
        write_bytes(
            new_payload.add(new_capacity + old_card_words),
            0,
            new_card_words - old_card_words,
        );
    }

    let free_entries = new_free.cast::<c_int>();
    for index in old_capacity..new_capacity - 1 {
        unsafe { free_entries.add(index).write((index + 1) as c_int) };
    }
    unsafe { free_entries.add(new_capacity - 1).write(old_free_head) };
}
