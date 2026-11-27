use core::ffi::{c_int, c_void};
use core::mem::{offset_of, size_of};

core::arch::global_asm!(".hidden ghc_rust_timeout_queue_insert");

// See Note [Timeout insertion offsets] in rts/TimeoutQueue.c.
#[repr(C)]
struct HeapSuffix {
    parent: *mut c_void,
    a: *mut c_void,
    b: *mut c_void,
    rank: u32,
    waketime: i64,
}

const _: () = {
    assert!(size_of::<HeapSuffix>() == 40);
    assert!(offset_of!(HeapSuffix, parent) == 0);
    assert!(offset_of!(HeapSuffix, a) == 8);
    assert!(offset_of!(HeapSuffix, b) == 16);
    assert!(offset_of!(HeapSuffix, rank) == 24);
    assert!(offset_of!(HeapSuffix, waketime) == 32);
};

unsafe fn suffix(node: *mut c_void, parent_offset: usize) -> *mut HeapSuffix {
    unsafe { node.cast::<u8>().add(parent_offset).cast::<HeapSuffix>() }
}

unsafe fn rank(node: *mut c_void, empty: *mut c_void, parent_offset: usize) -> u32 {
    if node == empty {
        0
    } else {
        unsafe { (&raw const (*suffix(node, parent_offset)).rank).read() }
    }
}

/// Insert one initialized timeout into a leftist heap.
///
/// # Safety
///
/// C owns the queue, all nodes, and the non-null empty sentinel. It gives this
/// call exclusive access to the queue. The new node has rank one and empty
/// children. `parent_offset` and the relative field offsets match the checked
/// C layout. The optional counter pointers are valid for this call.
#[unsafe(no_mangle)]
pub(super) unsafe extern "C" fn ghc_rust_timeout_queue_insert(
    root: *mut *mut c_void,
    new_node: *mut c_void,
    empty: *mut c_void,
    parent_offset: usize,
    insert_down_count: *mut c_int,
    leftify_count: *mut c_int,
) {
    let mut parent = empty;
    let mut parent_child = root;
    let mut node = unsafe { root.read() };
    let new_fields = unsafe { suffix(new_node, parent_offset) };
    let deadline = unsafe { (&raw const (*new_fields).waketime).read() };
    let mut down_steps = 0;

    loop {
        if node == empty {
            break;
        }
        let fields = unsafe { suffix(node, parent_offset) };
        let node_deadline = unsafe { (&raw const (*fields).waketime).read() };
        if deadline <= node_deadline {
            unsafe {
                (&raw mut (*new_fields).a).write(node);
                (&raw mut (*fields).parent).write(new_node);
            }
            break;
        }
        parent = node;
        parent_child = unsafe { &raw mut (*fields).b };
        node = unsafe { parent_child.read() };
        down_steps += 1;
    }

    unsafe {
        (&raw mut (*new_fields).parent).write(parent);
        parent_child.write(new_node);
    }

    let mut node = parent;
    let mut up_steps = 0;
    while node != empty {
        let fields = unsafe { suffix(node, parent_offset) };
        let left = unsafe { (&raw const (*fields).a).read() };
        let right = unsafe { (&raw const (*fields).b).read() };
        let left_rank = unsafe { rank(left, empty, parent_offset) };
        let right_rank = unsafe { rank(right, empty, parent_offset) };
        let rank_slot = unsafe { &raw mut (*fields).rank };
        let old_rank = unsafe { rank_slot.read() };

        let new_rank = if left_rank >= right_rank {
            right_rank + 1
        } else {
            unsafe {
                (&raw mut (*fields).a).write(right);
                (&raw mut (*fields).b).write(left);
            }
            left_rank + 1
        };
        if old_rank == new_rank {
            break;
        }
        unsafe { rank_slot.write(new_rank) };
        node = unsafe { (&raw const (*fields).parent).read() };
        up_steps += 1;
    }

    if !insert_down_count.is_null() {
        unsafe { *insert_down_count += down_steps };
    }
    if !leftify_count.is_null() {
        unsafe { *leftify_count += up_steps };
    }
}
