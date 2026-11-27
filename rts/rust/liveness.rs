use core::ptr::null_mut;
use core::sync::atomic::{AtomicPtr, Ordering, fence};

core::arch::global_asm!(".hidden ghc_rust_is_alive");

// GCAux.c checks these values against the native RTS headers.
const TAG_MASK: usize = 7;
const BF_EVACUATED: u16 = 1;
const BF_LARGE: u16 = 2;
const BF_MARKED: u16 = 8;
const BF_NONMOVING: u16 = 1024;
const IND: u32 = 27;
const IND_STATIC: u32 = 28;
const BLACKHOLE: u32 = 38;

// Query liveness before compaction. Follow each indirection with its own tag.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_is_alive(
    mut pointer: *mut u8,
    block_flags: unsafe extern "C" fn(*mut u8) -> *const u16,
    marked: unsafe extern "C" fn(*mut u8) -> bool,
    info_type_offset: isize,
    indirectee_offset: usize,
) -> *mut u8 {
    unsafe {
        loop {
            let closure = pointer.map_addr(|addr| addr & !TAG_MASK);
            let flags = block_flags(closure);
            if flags.is_null() {
                return pointer;
            }
            let flags = flags.read();
            if flags & (BF_NONMOVING | BF_EVACUATED) != 0 {
                return pointer;
            }
            if flags & BF_LARGE != 0 {
                return null_mut();
            }
            if flags & BF_MARKED != 0 && marked(closure) {
                return pointer;
            }

            let info = AtomicPtr::<u8>::from_ptr(closure.cast()).load(Ordering::Relaxed);
            if info.addr() & 1 != 0 {
                return info.map_addr(|addr| (addr - 1) | (pointer.addr() & TAG_MASK));
            }
            fence(Ordering::Acquire);
            let kind = info.wrapping_offset(info_type_offset).cast::<u32>().read();
            match kind {
                IND | IND_STATIC | BLACKHOLE => {
                    pointer = closure.add(indirectee_offset).cast::<*mut u8>().read();
                    if kind == BLACKHOLE && pointer.addr() & TAG_MASK == 0 {
                        return null_mut();
                    }
                }
                _ => return null_mut(),
            }
        }
    }
}
