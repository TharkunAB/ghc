# An incremental Rust port of the GHC RTS

Start with `__decodeFloat_Int` to prove that Hadrian can put a Rust object into a working RTS. Then port the in-process file-lock registry to prove that Rust can own an entire subsystem. Each step should leave a compiler that can build and run ordinary Haskell programs.

The source baseline is upstream GHC `56bb91d95f89509cb351a5eb4a7406825675866c`. The first target is native `x86_64-unknown-linux-gnu` with Rust 1.100.0. The mixed build is opt-in with `./configure --enable-rust-rts`. GHC normalizes the platform triple to `x86_64-unknown-linux` during configure. The probe uses Autoconf’s `target_cpu` and `target_os` to distinguish the GNU ABI. [Configure](../../configure.ac) · [Rust release history](https://blog.rust-lang.org/releases/)

## Current coverage

| Behavior | Evidence on x86-64 GNU/Linux | Next check |
| --- | --- | --- |
| `__decodeFloat_Int` | The Rust adapter and the original C function agreed on both output words for all 4,294,967,296 binary32 bit patterns. The standalone harness compiled the C source with a small header shim. | Exercise the decoder from GHC and libraries built by this branch. |
| Haskell `decodeFloat` | GHC 9.14.1 linked the branch’s adapter and Rust object into an optimized executable using its installed static RTS. It agreed with its C counterpart on 19,456 inputs. GDB reached Rust from `stg_decodeFloatzuIntzh`. The extended `num010` regression matched its golden output. | Repeat through Hadrian’s static, dynamic and threaded links. |
| Recursive `fib` | With the installed RTS and a Rust object built by the branch’s Hadrian rule, integer `fib` followed by Float decoding gave the expected results for 0, 1, 10 and 30. `fib 30` produced `832040`, which decoded as `(13312640,-4)`. | Repeat on the branch-built compiler and qualify later RTS changes with the same smoke test. |
| In-process file locks | The Rust and original C registries agreed on one million operations per way. The branch’s Float and file-lock objects together passed `openFile005`, `countReaders001`, `num010` and `fib` against the installed GHC 9.14.1 RTS in vanilla and threaded `-N4` programs. A separate hard-link and contention probe exercised shared file identity and release after the last reader. | Run the same cases against libraries packaged by this branch. |

The baseline has **180 tracked C translation units, 17 Cmm sources and 5 assembly sources** under `rts/`. As of the Float increment, 1 of 7 externally named functions in `StgPrimFloat.c` has a Rust implementation. Whole-file coverage is 0/180 C, 0/17 Cmm and 0/5 assembly. Count partial implementations by behavior. For a completed file, record the remaining ABI stubs and the targets and RTS ways in which the implementation ran.

| C subsystem | Tracked units |
| --- | ---: |
| Startup, embedding, utilities and hooks | 20 |
| STG bridge, foreign calls and bytecode execution | 13 |
| Primitive helpers | 15 |
| Scheduling, concurrency and STM | 14 |
| Heap representation, storage and GC | 34 |
| I/O, timers and platform services | 29 |
| Native linker and symbol metadata | 27 |
| Profiling, events and diagnostics | 22 |
| Wasm runtime C sources | 6 |

Use `git ls-tree -r --name-only 56bb91d95f89 rts` to reproduce the tracked source counts. The inventory also includes 230 headers (80 in `rts/include`) and 27 JavaScript sources. Track executable inline header code with its callers. The RTS package additionally builds `fs.c` from `utils/fs` and four generated `AutoApply` Cmm files. Record public layout, STG register or platform-specific assembly requirements with each affected unit. [RTS package](../../rts/rts.cabal) · [Generated code rules](../../hadrian/src/Rules/Generate.hs)

## 1. Put one Rust function on a real Haskell call path

For native code, `decodeFloat (x :: Float)` calls `decodeFloat_Int#`. Its Cmm entry point reserves two words on the STG stack and passes their addresses and a float to `__decodeFloat_Int`. The function writes the signed mantissa and exponent. It has no allocator, locks, callbacks or Haskell heap references. A Rust function can decode the bits into a pair of values. Only its FFI adapter needs to write through pointers. Leave the Double routines for a separate change. [Haskell instance](../../libraries/ghc-internal/src/GHC/Internal/Float.hs) · [Cmm caller](../../rts/PrimOps.cmm) · [C implementation](../../rts/StgPrimFloat.c) · [ABI declaration](../../rts/StgPrimFloat.h)

**Observed baseline.** On x86-64 Linux, installed GHC 9.14.1 compiled this probe with `-O2`. It read ten `Word32` bit patterns from the command line. The object referenced `stg_decodeFloatzuIntzh`, and GDB stopped in `__decodeFloat_Int` with that Cmm routine as its caller. Both signed zeros gave `(0,0)`. The smallest positive subnormal, `0x00000001`, gave `(8388608,-172)`. Runtime input matters because GHC can fold literal decodes. Repeat the check with the compiler built from this branch to qualify the port. [Constant-folding rule](../../compiler/GHC/Core/Opt/ConstantFold.hs) · [Native lowering](../../compiler/GHC/StgToCmm/Prim.hs)

```haskell
import Data.Word (Word32)
import GHC.Float (castWord32ToFloat)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mapM_ (print . decodeFloat . castWord32ToFloat . (read :: String -> Word32))
```

Compare the output of the C and mixed builds for `00000000 80000000 00000001 007fffff 00800000 3f800000 bf800000 7f7fffff 7f800000 7fc12345` (prefix each input with `0x`). These cover signed zeros, both ends of the subnormal range, the least normal, ±1, the largest finite value, infinity and a NaN with a payload. Compare nonfinite results against the C build as an implementation compatibility check. Set a breakpoint on the private Rust symbol once to verify the selected implementation ran.

### Build and rollback

1. Build the C reference from this revision in its own build directory. Initialize the pinned submodules and run `./boot` and `./configure` in the GHC build environment. Add an opt-in build setting for the native pilot. The default build must work with Rust absent. Report a configuration error if the user requests the Rust build for a target it cannot build.
2. Keep `__decodeFloat_Int` as the existing C entry point. In the opt-in build, have that wrapper call a private Rust symbol. Use the same tracked setting to select the wrapper body and the extra object. Preserve the Cmm and installed interfaces. Track the Rust source, toolchain, target and flags so changing any of them rebuilds the object.
3. Add an RTS object rule to Hadrian. Its `extraObjects` branch already supplies objects to both `Ar Pack` and `Ghc LinkHs`. For this allocation-free pilot, try one `#![no_std]` object from `rustc --emit=obj`, with PIC, one codegen unit and abort-on-panic. Keep the normal path panic-free. Use Hadrian’s stage target and the configured RTS build way. The Windows dynamic path already contributes an import library there. Audit undefined symbols, exports and relocations before linking. [Library rules](../../hadrian/src/Rules/Library.hs) · [RTS rules](../../hadrian/src/Rules/Rts.hs) · [Stage targets](../../hadrian/src/Oracles/Setting.hs) · [Rust compiler options](https://doc.rust-lang.org/rustc/codegen-options/index.html)
4. Use the resulting stage-2 GHC to compile, link and run the probe. Start with normal and threaded static RTS libraries and a dynamic executable. Check GHCi, then the enabled debug and profiling ways and an installed bindist before broadening support. Inspect the final archive and shared library for the expected symbols. Rebuild after a Rust edit. Switch the option off in the same build directory and check that the Rust reference and archive member disappear. A full Rust `staticlib` becomes worth evaluating when allocation or upstream crates enter the port. It carries additional objects and can require native final-link dependencies. [Default RTS ways](../../hadrian/src/Settings/Default.hs) · [Hadrian tests](../../hadrian/doc/testsuite.md) · [Rust linkage](https://doc.rust-lang.org/reference/linkage.html)

## 2. Give Rust a subsystem it can own

**The second increment ports the file-lock registry.** `lockFile` takes a handle ID, a device, an inode and a read/write flag. `unlockFile` takes the ID. The C implementation owns two indexes and reader counts. `openFile` already supplies the file identity. Keep the public functions, startup/shutdown hooks and existing RTS mutex in C for the first change. Give Rust one private registry behind an opaque pointer. Rust can represent file identities, reader counts and writer state directly. This avoids porting the generic RTS hash table. [File-lock contract](../../rts/include/rts/FileLock.h) · [Registry](../../rts/FileLock.c) · [Haskell caller](../../libraries/ghc-internal/src/GHC/Internal/IO/FD.hs)

The Rust registry owns both index lifetimes, the file identities, and the reader/writer transitions. It uses the existing RTS allocator and hash primitives. A repeated handle ID adds another binding. A bucket split in the C hash table can reverse the lookup order of such bindings. Reusing that table preserves the observed behavior. The C wrappers still own the mutex and startup/shutdown. Check ID width and the existing Haskell FFI signatures before extending to 32-bit targets. [Reader-count regression](../../libraries/base/tests/IO/countReaders001.hs) · [Lock combinations](../../libraries/base/tests/IO/openFile005.hs) · [Open/close workload](../../libraries/base/tests/IO/openFile008.hs)

## 3. Pick subsequent ports by ownership and callers

These work items can advance independently once the mixed build works. Each row identifies the part that can move and a Haskell or RTS check that would exercise it.

| Priority | Candidate | Boundary and acceptance check |
| --- | --- | --- |
| Early | [Profiling JSON escaping](../../rts/ProfilerReportJson.c) | Port byte escaping into caller-owned output. Keep cost-centre stack traversal and report I/O in C. Compare escaped bytes, including controls and the final NUL. Run a profiled program with quoted arguments and parse the `+RTS -pj` report. |
| Early | [Eventlog file writer](../../rts/eventlog/EventLogWriter.c) | The [writer callback table](../../rts/include/rts/EventLogWriter.h) already separates byte production from delivery. Preserve concurrent writes and flushes, short writes, restart, fork naming and explicit `-ol` paths. Run `InitEventLogging`, `RestartEventLogging` and the filename tests, then decode a known `traceEventIO` marker from a threaded run. |
| Early, linker-specific | [Archive member format detection](../../rts/linker/LoadArchive.c) | `identifyObjectFile_` takes a bounded byte prefix. Return a format tag while C owns the stream, mappings, symbols and relocations. Compare a corpus of ELF, Mach-O and COFF members and short prefixes, then load a real static archive in GHCi. |
| Middle | [GC sizing policy](../../rts/sm/GC.c) | Start with the arithmetic in `resizeGenerations`. C snapshots sizes and flags at the existing synchronization point and applies the result. Compare decisions around `-M`, generation count, compaction and nonmoving mode. Then compare GC statistics on a fixed allocation workload. |
| Middle | [Megablock free extents](../../rts/sm/MBlock.c) | Move the large-address-space free-range index, selection and coalescing together. C owns OS commit/decommit, NUMA placement and heap-membership queries. Run `testmblockalloc` and `testblockalloc`. Exercise reuse after freeing adjacent ranges. |
| Middle | [POSIX poll bookkeeping](../../rts/posix/Poll.c) | Rust can return ready slot indices and flags. C owns the corresponding [closure table](../../rts/ClosureTable.h), wakes threads, and performs GC barriers. Preserve index alignment on swap-removal. Exercise the poll I/O manager explicitly, including readiness, timeout and cancellation. |
| Later | [Work-stealing deque](../../rts/WSDeque.h) | First give the C implementation operations for pruning and visiting slots while stealing is suspended. [Spark GC](../../rts/Sparks.c) reads and rewrites the array directly. Once it uses those operations, Rust can own the array and atomics. Run `testwsdeque`, then exercise spark pruning, slot writeback during moving GC and parallel GC work distribution. Measure contention and GC time. |

For a port on a hot path, measure the C and mixed builds with the same revision, optimization, RTS flags and workload before moving the next caller. Extend target coverage separately from subsystem coverage. Native AArch64, Windows, 32-bit and Wasm each need a target-specific build and run. The JavaScript backend has its own RTS and its own lowering for the Float operation. [Platform RTS sources](../../rts/rts.cabal) · [JavaScript lowering](../../compiler/GHC/StgToJS/Prim.hs)

## 4. Prepare the boundaries for the core runtime

- **Hash and arena.** The generic [hash table](../../rts/Hash.h) accepts duplicate keys, custom hash functions and callbacks that can rewrite key slots during [compaction](../../rts/sm/Compact.c). The [arena](../../rts/Arena.c) acquires RTS block groups and contributes to GC block accounting. Move a consumer with private state first. These two modules need their own caller-by-caller contracts.
- **Stable pointers and names.** Cmm reads table globals, and the installed [stable-pointer header](../../rts/include/rts/StablePtr.h) inlines lookup. Preserve existing compiled clients, publication ordering, old table lifetime during growth, moving-GC root updates and weak identity. An opaque Rust table requires an explicit transition for these callers. [Cmm operations](../../rts/PrimOps.cmm) · [Stable pointers](../../rts/StablePtr.c) · [Stable names](../../rts/StableName.c)
- **GHCi bytecode and foreign callbacks.** The bytecode interpreter shares the STG stack and hands execution back to the scheduler on heap and stack overflow. Callback adjustors combine a platform calling convention with a stable pointer. Keep these entry paths intact while porting private helpers. For a later interpreter or adjustor port, test GHCi bytecode calling compiled Haskell and a C caller invoking and releasing a Haskell callback. [Interpreter](../../rts/Interpreter.c) · [Adjustors](../../rts/Adjustor.c)
- **Scheduler, STM and collectors.** A capability contains the STG register table. Generated code uses offsets derived from C layouts. Safe foreign calls release and reacquire a capability. The collectors trace stacks, transaction records and queues and can update their pointer slots. Take one protocol at a time, starting with GC-root visitation or a scheduler state transition. Establish the synchronization point, pointer lifetime and write barrier before translating it. Retain Cmm and assembly entry stubs wherever generated code requires the STG calling convention. Qualify on moving and nonmoving GC, parallel execution, safe FFI callbacks, async exceptions and STM retry before switching a core subsystem. [Capability layout](../../rts/Capability.h) · [Derived constants](../../utils/deriveConstants/Main.hs) · [Scheduler](../../rts/Schedule.c) · [STM](../../rts/STM.c) · [STG entry](../../rts/StgCRun.c)

Keep each temporary C ABI at a subsystem boundary. Rust-owned data can use ordinary Rust types. Use `repr(C)` only for records that cross the ABI by value or whose layout foreign code reads. A collector can relocate a Haskell object while Rust still holds its old address. Pass an RTS-managed handle or a visitable slot when a pointer must survive collection. [Rust FFI guidance](https://doc.rust-lang.org/nomicon/ffi.html)

Edward Yang’s earlier GHC RTS experiment contains a Rust block allocator. Junming Zhao’s GHC–MMTk work uncovered an assumption about the stack frame of a dead thread. GHC introduced a dedicated frame after that finding. Use that case when designing stack-scanning tests. MMTk lists GHC as a third-party binding. [GHC RTS in Rust](https://github.com/ezyang/ghc-rts-rust) · [GHC–MMTk talk](https://www.youtube.com/watch?v=3XPygGpuP9M) · [GHC stack-frame change](https://github.com/ghc/ghc/commit/5630dde68185f96da026a4e0c722fe6631633299) · [MMTk status](https://www.mmtk.io/status)

Mozilla’s `mp4parse` provides a smaller design example. The parser owns its Rust types and behavior. A separate C adapter exposes the operations Firefox needs. The file-lock registry can follow the same pattern. [mp4parse](https://github.com/mozilla/mp4parse-rust)
