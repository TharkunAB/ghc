#include "rts/PosixSource.h"
#include "Rts.h"

#include "AllocArray.h"

#if defined(USE_RUST_RTS) && !defined(TSAN_ENABLED)
#define USE_RUST_ARRAY_ALLOC
GHC_STATIC_ASSERT(sizeof(StgWord) == sizeof(size_t) &&
                  sizeof(StgArrBytes) % sizeof(StgWord) == 0,
                  "Rust byte array allocation requires a whole-word header");
GHC_STATIC_ASSERT(offsetof(StgMutArrPtrs, payload) == sizeof(StgMutArrPtrs) &&
                  offsetof(StgMutArrPtrs, size) + sizeof(StgWord) == sizeof(StgMutArrPtrs) &&
                  offsetof(StgMutArrPtrs, ptrs) + 2 * sizeof(StgWord) == sizeof(StgMutArrPtrs) &&
                  offsetof(StgSmallMutArrPtrs, payload) == sizeof(StgSmallMutArrPtrs) &&
                  offsetof(StgSmallMutArrPtrs, ptrs) + sizeof(StgWord) == sizeof(StgSmallMutArrPtrs) &&
                  sizeof(StgClosure *) == sizeof(StgWord),
                  "Rust pointer arrays require word-aligned metadata before the payload");
RTS_PRIVATE StgMutArrPtrs *ghc_rust_allocate_mut_array(
    Capability *cap, StgWord count, StgClosure *init, CostCentreStack *ccs, size_t header_bytes);
RTS_PRIVATE StgSmallMutArrPtrs *ghc_rust_allocate_small_mut_array(
    Capability *cap, StgWord count, StgClosure *init, CostCentreStack *ccs, size_t header_bytes);
RTS_PRIVATE StgArrBytes *ghc_rust_allocate_byte_array(
    Capability *cap, StgWord bytes, size_t header_bytes);
RTS_PRIVATE StgArrBytes *ghc_rust_allocate_pinned_byte_array(
    Capability *cap, StgWord bytes, StgWord alignment, size_t header_bytes);
#endif

#if defined(USE_RUST_RTS)
/* The Rust object is common to all RTS ways. Use this way's profiling macros. */
RTS_PRIVATE void ghc_rust_set_array_header(StgClosure *arr, const StgInfoTable *info,
                                           CostCentreStack *ccs,
                                           size_t header_words, size_t payload_words);
void ghc_rust_set_array_header(StgClosure *arr, const StgInfoTable *info,
                               CostCentreStack *ccs USED_IF_PROFILING,
                               size_t header_words STG_UNUSED, size_t payload_words STG_UNUSED)
{
    TICK_ALLOC_PRIM(header_words, payload_words, 0);
    SET_HDR(arr, info, ccs);
}
#endif

/* Note [Array allocation sizes]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Reject payloads that cannot fit in the address space with their header.
 * This keeps the card count, word rounding, and header addition in range.
 * The block allocator checks its own limits on the resulting word count.
 */
StgMutArrPtrs *allocateMutArrPtrs (Capability *cap,
                                   StgWord nelements,
                                   StgClosure *init,
                                   CostCentreStack *ccs USED_IF_PROFILING)
{
#if defined(USE_RUST_ARRAY_ALLOC)
    return ghc_rust_allocate_mut_array(cap, nelements, init, ccs, sizeof(StgMutArrPtrs));
#else
    if (RTS_UNLIKELY(nelements > (HS_WORD_MAX - sizeof(StgMutArrPtrs)) / sizeof(StgClosure *))) return NULL;

    /* All sizes in words */

    /* The card table contains one byte for each 2^MUT_ARR_PTRS_CARD_BITS words
     * in the array, making sure we round up, and then rounding up to a whole
     * number of words. */
    StgWord cardsize = mutArrPtrsCardTableSize(nelements); /* card table */
    StgWord arrsize  = nelements + cardsize;               /* +array size */
    StgWord objsize  = sizeofW(StgMutArrPtrs) + arrsize;   /* +header size */
    StgMutArrPtrs *arr;
    arr = (StgMutArrPtrs *)allocateMightFail(cap, objsize);
    if (RTS_UNLIKELY(arr == NULL)) return NULL;
    TICK_ALLOC_PRIM(sizeofW(StgMutArrPtrs), arrsize, 0);

    /* No write barrier needed since this is a new allocation. */
    SET_HDR(arr, &stg_MUT_ARR_PTRS_DIRTY_info, ccs);
    arr->ptrs = nelements;
    arr->size = arrsize;

    if (init != NULL) {
        for (StgWord i = 0; i < nelements; ++i) {
            arr->payload[i] = init;
        }
    }

    /* Initialize the card array. Note that memset needs sizes in bytes. */
    memset(&(arr->payload[nelements]), 0, mutArrPtrsCards(nelements));

    return arr;
#endif
}

StgSmallMutArrPtrs *allocateSmallMutArrPtrs (Capability *cap,
                                             StgWord nelements,
                                             StgClosure *init,
                                             CostCentreStack *ccs
                                               USED_IF_PROFILING)
{
#if defined(USE_RUST_ARRAY_ALLOC)
    return ghc_rust_allocate_small_mut_array(cap, nelements, init, ccs, sizeof(StgSmallMutArrPtrs));
#else
    if (RTS_UNLIKELY(nelements > (HS_WORD_MAX - sizeof(StgSmallMutArrPtrs)) / sizeof(StgClosure *))) return NULL;

    /* All sizes in words */
    StgWord arrsize = nelements;                              /* array size */
    StgWord objsize = sizeofW(StgSmallMutArrPtrs) + arrsize;  /* +header size */
    StgSmallMutArrPtrs *arr;
    arr = (StgSmallMutArrPtrs *)allocateMightFail(cap, objsize);
    if (RTS_UNLIKELY(arr == NULL)) return NULL;
    TICK_ALLOC_PRIM(sizeofW(StgSmallMutArrPtrs), arrsize, 0);

    /* No write barrier needed since this is a new allocation. */
    SET_HDR(arr, &stg_SMALL_MUT_ARR_PTRS_DIRTY_info, ccs);
    arr->ptrs = nelements;

    if (init != NULL) {
        for (StgWord i = 0; i < nelements; ++i) {
            arr->payload[i] = init;
        }
    }

    return arr;
#endif
}

/* The header and allocation counters depend on the current RTS way. */
static inline StgArrBytes *finishByteArray(StgArrBytes *arr, StgWord bytes,
                                          CostCentreStack *ccs USED_IF_PROFILING)
{
    if (RTS_UNLIKELY(arr == NULL)) return NULL;
    TICK_ALLOC_PRIM(sizeofW(StgArrBytes), ROUNDUP_BYTES_TO_WDS(bytes), 0);
    SET_HDR(arr, &stg_ARR_WORDS_info, ccs);
    arr->bytes = bytes;
    return arr;
}

StgArrBytes *allocateArrBytes (Capability *cap,
                               StgWord arrbytes,
                               CostCentreStack *ccs USED_IF_PROFILING)
{
    StgArrBytes *arr;
#if defined(USE_RUST_ARRAY_ALLOC)
    arr = ghc_rust_allocate_byte_array(cap, arrbytes, sizeof(StgArrBytes));
#else
    if (RTS_UNLIKELY(arrbytes > HS_WORD_MAX - sizeof(StgArrBytes))) return NULL;
    StgWord objsize = sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(arrbytes);
    arr = (StgArrBytes *)allocateMightFail(cap, objsize);
#endif
    return finishByteArray(arr, arrbytes, ccs);
}

StgArrBytes *allocateArrBytesPinned (Capability *cap,
                                     StgWord arrbytes,
                                     StgWord alignment,
                                     CostCentreStack *ccs USED_IF_PROFILING)
{
    StgArrBytes *arr;
#if defined(USE_RUST_ARRAY_ALLOC)
    arr = ghc_rust_allocate_pinned_byte_array(cap, arrbytes, alignment, sizeof(StgArrBytes));
#else
    if (RTS_UNLIKELY(arrbytes > HS_WORD_MAX - sizeof(StgArrBytes))) return NULL;
    if (alignment <= sizeof(StgWord)) { alignment = sizeof(StgWord); }

    StgWord objsize = sizeofW(StgArrBytes) + ROUNDUP_BYTES_TO_WDS(arrbytes);
    /* The payload starts after the header and must satisfy the alignment. */
    arr = (StgArrBytes *)allocatePinned(cap, objsize, alignment, sizeof(StgArrBytes));
#endif
    return finishByteArray(arr, arrbytes, ccs);
}
