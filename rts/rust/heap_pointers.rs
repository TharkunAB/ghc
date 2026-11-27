#![no_std]
use core::ffi::{c_char, c_void};
use core::mem::size_of;
use core::ptr;
use core::sync::atomic::{AtomicPtr, Ordering};
include!(env!("HEAP_POINTER_LAYOUT_RS"));
core::arch::global_asm!(
    ".hidden ghc_rust_collect_pointers\n.hidden ghc_rust_heap_view_closure_ptrs"
);
type Closure = *mut u8;
unsafe extern "C" {
    fn stgMallocBytes(bytes: usize, message: *const c_char) -> *mut c_void;
    fn stgFree(ptr: *mut c_void);
    fn collect_pointers(c: Closure, out: *mut Closure) -> usize;
    fn allocateMutArrPtrs(cap: *mut u8, count: usize, init: Closure, ccs: *mut u8) -> *mut u8;
    static stg_MUT_ARR_PTRS_FROZEN_CLEAN_info: u8;
    fn heap_view_closure_ptrs_in_pap_payload(
        out: *mut Closure,
        n: *mut usize,
        fun: Closure,
        payload: *const Closure,
        size: usize,
    );
    fn ghc_heap_checked_tso(c: Closure, out: *mut Closure) -> usize;
    fn ghc_heap_unknown_type(kind: u32);
    fn barf(format: *const c_char, ...) -> !;
}

// The closure size bounds the temporary array. Scan before allocating the
// result so the allocator receives the exact pointer count and current CCS.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_heap_view_closure_ptrs(
    cap: *mut u8,
    closure: Closure,
    size: usize,
) -> *mut u8 {
    unsafe {
        let temp = stgMallocBytes(
            size.wrapping_mul(size_of::<Closure>()),
            c"heap_view_closurePtrs".as_ptr(),
        )
        .cast::<Closure>();
        let count = collect_pointers(closure, temp);
        let ccs = cap.add(CAPABILITY_CCCS).cast::<*mut u8>().read();
        let array = allocateMutArrPtrs(cap, count, ptr::null_mut(), ccs);
        if !array.is_null() {
            array
                .add(ARRAY_INFO)
                .cast::<*const u8>()
                .write(ptr::addr_of!(stg_MUT_ARR_PTRS_FROZEN_CLEAN_info));
            if count != 0 {
                ptr::copy_nonoverlapping(temp, array.add(ARRAY_PAYLOAD).cast::<Closure>(), count);
            }
        }
        stgFree(temp.cast());
        array
    }
}

#[inline]
unsafe fn field(c: Closure, off: usize) -> Closure {
    unsafe { c.add(off).cast::<Closure>().read() }
}
#[inline]
unsafe fn take_fields<const N: usize>(c: Closure, out: *mut Closure, offsets: [usize; N]) -> usize {
    unsafe {
        for (slot, offset) in offsets.into_iter().enumerate() {
            out.add(slot).write(field(c, offset));
        }
    }
    N
}
#[inline]
unsafe fn optional(c: Closure, out: *mut Closure, count: usize, offset: usize) -> usize {
    let x = unsafe { field(c, offset) };
    if x.is_null() {
        count
    } else {
        unsafe { out.add(count).write(x) };
        count + 1
    }
}
#[inline]
unsafe fn take_payload(c: Closure, out: *mut Closure, start: usize, count: usize) -> usize {
    if count != 0 {
        unsafe {
            let source = c.add(start).cast::<Closure>();
            match count {
                1 => out.write(source.read()),
                2 => ptr::copy(source, out, 2),
                _ => ptr::copy(source, out, count),
            }
        }
    }
    count
}
#[inline]
unsafe fn application(
    c: Closure,
    out: *mut Closure,
    fun_off: usize,
    n_off: usize,
    payload_off: usize,
) -> usize {
    unsafe {
        let fun = field(c, fun_off);
        out.write(fun);
        let n_args = c.add(n_off).cast::<u32>().read() as usize;
        let mut count = 1;
        heap_view_closure_ptrs_in_pap_payload(
            out,
            &mut count,
            fun,
            c.add(payload_off).cast(),
            n_args,
        );
        count
    }
}
// Note [Heap pointer traversal]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// The caller keeps the closure in place and supplies an output slot for each
// pointer. Field offsets and tags come from the headers for this RTS way.
// Copy pointers in the order expected by ghc-heap, including their tag bits.
// An AP_STACK contributes its function only. A continuation contributes no
// pointers because its saved stack contains both pointers and nonpointers.
#[unsafe(no_mangle)]
unsafe extern "C" fn ghc_rust_collect_pointers(
    c: Closure,
    out: *mut Closure,
    info: *const u8,
) -> usize {
    unsafe {
        let kind = info.add(INFO_TYPE).cast::<u32>().read();
        match kind {
            TAG_INVALID_OBJECT => barf(c"Invalid Object".as_ptr()),
            TAG_ARR_WORDS | TAG_STACK | TAG_CONTINUATION => 0,
            TAG_CONSTR_1_0 | TAG_CONSTR_0_1 | TAG_CONSTR_2_0 | TAG_CONSTR_1_1 | TAG_CONSTR_0_2
            | TAG_CONSTR | TAG_CONSTR_NOCAF | TAG_PRIM | TAG_FUN | TAG_FUN_0_1 | TAG_FUN_1_0
            | TAG_FUN_1_1 | TAG_FUN_2_0 | TAG_FUN_0_2 | TAG_FUN_STATIC => take_payload(
                c,
                out,
                PAYLOAD,
                info.add(INFO_PTRS).cast::<u32>().read() as usize,
            ),
            TAG_THUNK | TAG_THUNK_1_0 | TAG_THUNK_0_1 | TAG_THUNK_1_1 | TAG_THUNK_2_0
            | TAG_THUNK_0_2 | TAG_THUNK_STATIC => take_payload(
                c,
                out,
                THUNK_PAYLOAD,
                info.add(INFO_PTRS).cast::<u32>().read() as usize,
            ),
            TAG_THUNK_SELECTOR => take_fields(c, out, [SELECTEE]),
            TAG_AP => application(c, out, AP_FUN, AP_ARGS, AP_PAYLOAD),
            TAG_PAP => application(c, out, PAP_FUN, PAP_ARGS, PAP_PAYLOAD),
            TAG_AP_STACK => take_fields(c, out, [AP_STACK_FUN]),
            TAG_BCO => take_fields(c, out, [BCO_INSTRS, BCO_LITERALS, BCO_PTRS]),
            TAG_IND | TAG_IND_STATIC | TAG_BLACKHOLE => {
                let target = AtomicPtr::from_ptr(c.add(INDIRECTEE).cast::<Closure>())
                    .load(Ordering::Acquire);
                out.write(target);
                1
            }
            TAG_MUT_ARR_PTRS_CLEAN
            | TAG_MUT_ARR_PTRS_DIRTY
            | TAG_MUT_ARR_PTRS_FROZEN_CLEAN
            | TAG_MUT_ARR_PTRS_FROZEN_DIRTY => take_payload(
                c,
                out,
                ARRAY_PAYLOAD,
                c.add(ARRAY_PTRS).cast::<usize>().read(),
            ),
            TAG_SMALL_MUT_ARR_PTRS_CLEAN
            | TAG_SMALL_MUT_ARR_PTRS_DIRTY
            | TAG_SMALL_MUT_ARR_PTRS_FROZEN_CLEAN
            | TAG_SMALL_MUT_ARR_PTRS_FROZEN_DIRTY => take_payload(
                c,
                out,
                SMALL_PAYLOAD,
                c.add(SMALL_PTRS).cast::<usize>().read(),
            ),
            TAG_MUT_VAR_CLEAN | TAG_MUT_VAR_DIRTY => take_fields(c, out, [MUT_VAR]),
            TAG_MVAR_CLEAN | TAG_MVAR_DIRTY => {
                take_fields(c, out, [MVAR_HEAD, MVAR_TAIL, MVAR_VALUE])
            }
            TAG_TSO if CHECK_TSO != 0 => ghc_heap_checked_tso(c, out),
            TAG_TSO => {
                let count = take_fields(
                    c,
                    out,
                    [
                        TSO_LINK,
                        TSO_GLOBAL,
                        TSO_STACK,
                        TSO_TREC,
                        TSO_EXCEPTIONS,
                        TSO_BQ,
                    ],
                );
                optional(c, out, count, TSO_LABEL)
            }
            TAG_WEAK => {
                let count = take_fields(
                    c,
                    out,
                    [WEAK_CFINALIZERS, WEAK_KEY, WEAK_VALUE, WEAK_FINALIZER],
                );
                optional(c, out, count, WEAK_LINK)
            }
            TAG_BLOCKING_QUEUE => take_fields(c, out, [BQ_LINK, BQ_BH, BQ_OWNER, BQ_QUEUE]),
            _ => {
                ghc_heap_unknown_type(kind);
                0
            }
        }
    }
}
