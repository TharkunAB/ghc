use super::{
    CAP_CURRENT_SEGMENTS, CENSUS_ACTIVE, CENSUS_BLOCKS, CENSUS_COLLECTED, CENSUS_FILLED,
    CENSUS_SIZE, CENSUS_WORDS, NM_ACTIVE, NM_BLOCK_COUNT, NM_BLOCK_SIZE, NM_FILLED, SEG_BITMAP,
    SEG_LINK, SEG_STRUCT_SIZE, closure_words,
};
use core::mem::{offset_of, size_of};
use core::sync::atomic::{AtomicPtr, AtomicU8, Ordering};

core::arch::global_asm!(".hidden ghc_rust_stopped_allocator_census");

#[repr(C)]
struct NonmovingAllocCensus {
    collected_live_words: bool,
    n_active_segs: u32,
    n_filled_segs: u32,
    n_live_blocks: u32,
    n_live_words: u32,
}

const _: () = {
    assert!(size_of::<NonmovingAllocCensus>() == CENSUS_SIZE);
    assert!(offset_of!(NonmovingAllocCensus, collected_live_words) == CENSUS_COLLECTED);
    assert!(offset_of!(NonmovingAllocCensus, n_active_segs) == CENSUS_ACTIVE);
    assert!(offset_of!(NonmovingAllocCensus, n_filled_segs) == CENSUS_FILLED);
    assert!(offset_of!(NonmovingAllocCensus, n_live_blocks) == CENSUS_BLOCKS);
    assert!(offset_of!(NonmovingAllocCensus, n_live_words) == CENSUS_WORDS);
};

#[inline]
unsafe fn read_ptr(base: *const u8, offset: usize) -> *mut u8 {
    unsafe { base.add(offset).cast::<*mut u8>().read() }
}

#[inline]
fn first_block(seg: *mut u8, block_count: usize) -> *const u8 {
    let align = size_of::<usize>();
    seg.map_addr(|address| {
        address
            .wrapping_add(SEG_STRUCT_SIZE)
            .wrapping_add(block_count)
            .wrapping_add(align - 1)
            & !(align - 1)
    })
    .cast_const()
}

#[inline]
unsafe fn marked_segment(
    seg: *mut u8,
    first: *const u8,
    block_size: usize,
    block_count: usize,
    epoch: u8,
) -> (u32, u32) {
    let mut blocks = 0u32;
    let mut words = 0u32;
    let marks = unsafe { seg.add(SEG_BITMAP).cast::<AtomicU8>() };
    for index in 0..block_count {
        let mark = unsafe { (*marks.add(index)).load(Ordering::Relaxed) };
        if mark != 0 && (epoch == 0 || mark == epoch) {
            blocks = blocks.wrapping_add(1);
            words = words.wrapping_add(unsafe { closure_words(first.add(index * block_size)) });
        }
    }
    (blocks, words)
}

/// Count the stopped nonmoving allocator's filled, active, and current segments.
/// The caller stops mutators and supplies the stable capability array.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_stopped_allocator_census(
    alloc: *const u8,
    alloc_idx: u32,
    caps: *const *mut u8,
    n_caps: u32,
    epoch: u8,
    out: *mut NonmovingAllocCensus,
) {
    unsafe {
        let block_size = alloc.add(NM_BLOCK_SIZE).cast::<u16>().read() as usize;
        let block_count = alloc.add(NM_BLOCK_COUNT).cast::<u16>().read() as usize;
        let mut census = NonmovingAllocCensus {
            collected_live_words: true,
            n_active_segs: 0,
            n_filled_segs: 0,
            n_live_blocks: 0,
            n_live_words: 0,
        };

        let mut seg = read_ptr(alloc, NM_FILLED);
        while !seg.is_null() {
            census.n_filled_segs = census.n_filled_segs.wrapping_add(1);
            census.n_live_blocks = census.n_live_blocks.wrapping_add(block_count as u32);
            let first = first_block(seg, block_count);
            for index in 0..block_count {
                census.n_live_words = census
                    .n_live_words
                    .wrapping_add(closure_words(first.add(index * block_size)));
            }
            seg = read_ptr(seg, SEG_LINK);
        }

        seg = read_ptr(alloc, NM_ACTIVE);
        while !seg.is_null() {
            census.n_active_segs = census.n_active_segs.wrapping_add(1);
            let (blocks, words) = marked_segment(
                seg,
                first_block(seg, block_count),
                block_size,
                block_count,
                epoch,
            );
            census.n_live_blocks = census.n_live_blocks.wrapping_add(blocks);
            census.n_live_words = census.n_live_words.wrapping_add(words);
            seg = read_ptr(seg, SEG_LINK);
        }

        for index in 0..n_caps as usize {
            let cap = AtomicPtr::from_ptr(caps.add(index).cast_mut()).load(Ordering::Relaxed);
            let current = read_ptr(cap, CAP_CURRENT_SEGMENTS).cast::<*mut u8>();
            seg = current.add(alloc_idx as usize).read();
            let (blocks, words) = marked_segment(
                seg,
                first_block(seg, block_count),
                block_size,
                block_count,
                0,
            );
            census.n_live_blocks = census.n_live_blocks.wrapping_add(blocks);
            census.n_live_words = census.n_live_words.wrapping_add(words);
        }
        out.write(census);
    }
}
