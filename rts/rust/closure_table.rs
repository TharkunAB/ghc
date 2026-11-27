use core::ffi::{c_int, c_void};
use core::mem::{offset_of, size_of};
use core::ptr::{copy_nonoverlapping, write_bytes};

core::arch::global_asm!(
    ".hidden ghc_rust_closure_table_init_growth",
    ".hidden ghc_rust_closure_table_init",
    ".hidden ghc_rust_closure_table_mark",
    ".hidden ghc_rust_closure_table_insert",
);

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

// See Note [Closure table Rust layout] in rts/ClosureTable.c.
#[repr(C)]
pub(super) struct ClosureTable {
    arr: *mut c_void,
    free: *mut c_void,
    free_hd: c_int,
    elems: c_int,
    capacity: c_int,
    compact: c_int,
}

unsafe extern "C" {
    static stg_MUT_ARR_PTRS_DIRTY_info: u8;
}

const _: () = {
    assert!(offset_of!(ClosureTable, arr) == 0);
    assert!(offset_of!(ClosureTable, free) == size_of::<*mut c_void>());
    assert!(offset_of!(ClosureTable, free_hd) == 2 * size_of::<*mut c_void>());
    assert!(offset_of!(ClosureTable, elems) == 2 * size_of::<*mut c_void>() + size_of::<c_int>());
    assert!(
        offset_of!(ClosureTable, capacity) == 2 * size_of::<*mut c_void>() + 2 * size_of::<c_int>()
    );
    assert!(
        offset_of!(ClosureTable, compact) == 2 * size_of::<*mut c_void>() + 3 * size_of::<c_int>()
    );
    assert!(size_of::<ClosureTable>() == 2 * size_of::<*mut c_void>() + 4 * size_of::<c_int>());
};

/// Initialize one table with the C-owned null closure sentinel.
///
/// # Safety
///
/// `table` is a valid, exclusively owned ClosureTable. C checks its layout.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_closure_table_init(
    table: *mut ClosureTable,
    mode: c_int,
    sentinel: *mut c_void,
) {
    unsafe {
        (&raw mut (*table).arr).write(sentinel);
        (&raw mut (*table).free).write(sentinel);
        (&raw mut (*table).free_hd).write(-1);
        (&raw mut (*table).elems).write(0);
        (&raw mut (*table).capacity).write(0);
        (&raw mut (*table).compact).write(mode);
    }
}

/// Visit both live root slots in the same order as C.
///
/// # Safety
///
/// `table` remains valid during both callbacks. The collector may replace
/// either root pointer through its slot. C checks the table layout.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_closure_table_mark(
    table: *mut ClosureTable,
    evac: unsafe extern "C" fn(*mut c_void, *mut *mut c_void),
    user: *mut c_void,
) {
    unsafe {
        evac(user, &raw mut (*table).arr);
        evac(user, &raw mut (*table).free);
    }
}

/// Insert one closure into a free slot and update its GC metadata.
///
/// # Safety
///
/// C owns the table and checks that its free head names an empty slot. C
/// supplies the live payload, card, and info slots using RTS array geometry.
/// `next_free` is the current free-list link. C checks the table layout and
/// marks the used free-list entry in DEBUG builds.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_closure_table_insert(
    table: *mut ClosureTable,
    value: *mut c_void,
    slot: *mut *mut c_void,
    card: *mut u8,
    info_slot: *mut *const c_void,
    next_free: c_int,
) -> c_int {
    let ix = unsafe { (&raw const (*table).free_hd).read() };
    unsafe {
        slot.write(value);
        card.write(1);
        info_slot.write((&raw const stg_MUT_ARR_PTRS_DIRTY_info).cast());
        (&raw mut (*table).free_hd).write(next_free);
        let elems = &raw mut (*table).elems;
        elems.write(elems.read() + 1);
    }
    ix
}

unsafe extern "C" {
    static stg_CLOSURE_TABLE_NULL_closure: u8;
}

core::arch::global_asm!(
    ".hidden ghc_rust_closure_table_remove_sparse",
    ".hidden ghc_rust_closure_table_remove_compact",
);

/// Link the released index at the head of the free list.
unsafe fn recycle(table: *mut ClosureTable, free_link: *mut c_int, ix: c_int) {
    unsafe {
        free_link.write((*table).free_hd);
        (*table).free_hd = ix;
        (*table).elems -= 1;
    }
}

/// Clear an occupied slot. C runs the nonmoving write barrier first.
///
/// # Safety
///
/// The caller owns the table. The slot and free link belong to index `ix`.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_closure_table_remove_sparse(
    table: *mut ClosureTable,
    slot: *mut *mut c_void,
    free_link: *mut c_int,
    ix: c_int,
) {
    unsafe {
        slot.write(
            (&raw const stg_CLOSURE_TABLE_NULL_closure)
                .cast_mut()
                .cast(),
        );
        recycle(table, free_link, ix);
    }
}

/// Move the last entry into the gap. Mark the destination card even when
/// the caller removes the last entry itself.
///
/// # Safety
///
/// The caller owns the table and has applied the nonmoving write barrier.
/// Both indexes are occupied. The free link belongs to the last slot and
/// the card belongs to the gap.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_closure_table_remove_compact(
    table: *mut ClosureTable,
    payload: *mut *mut c_void,
    free_link: *mut c_int,
    gap: c_int,
    last: c_int,
    gap_card: *mut u8,
) {
    unsafe {
        let last_slot = payload.add(last as usize);
        payload.add(gap as usize).write(last_slot.read());
        last_slot.write(
            (&raw const stg_CLOSURE_TABLE_NULL_closure)
                .cast_mut()
                .cast(),
        );
        gap_card.write(1);
        recycle(table, free_link, last);
    }
}
