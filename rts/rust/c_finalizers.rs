use core::ffi::c_void;

core::arch::global_asm!(".hidden ghc_rust_run_c_finalizers");

// The C caller supplies offsets for the current RTS way.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_run_c_finalizers(
    mut head: *mut u8,
    end: *mut u8,
    link_offset: usize,
    fptr_offset: usize,
    ptr_offset: usize,
    eptr_offset: usize,
    flag_offset: usize,
) {
    while head != end {
        unsafe {
            let flag = head.add(flag_offset).cast::<usize>().read();
            let fptr = head
                .add(fptr_offset)
                .cast::<unsafe extern "C" fn()>()
                .read();
            let ptr = head.add(ptr_offset).cast::<*mut c_void>().read();
            if flag != 0 {
                let eptr = head.add(eptr_offset).cast::<*mut c_void>().read();
                let call: unsafe extern "C" fn(*mut c_void, *mut c_void) =
                    core::mem::transmute(fptr);
                call(eptr, ptr);
            } else {
                let call: unsafe extern "C" fn(*mut c_void) = core::mem::transmute(fptr);
                call(ptr);
            }
            // Read the link after the callback, as the C loop does.
            head = head.add(link_offset).cast::<*mut u8>().read();
        }
    }
}
