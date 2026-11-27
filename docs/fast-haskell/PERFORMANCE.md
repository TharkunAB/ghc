# Incremental GHC self-builds

Use ordinary GHC for final builds. At each development-worker switch, compare with the exact GHC executable behind that worker. At each GHC or RTS switch, use an unmodified build of the same source revision, flavour and toolchain. Record revisions, workload, sample count, results and the reason for the switch. Include memory for changes that retain data or change the heap. Each worker release keeps its own `PERFORMANCE.md`.

## 27 November 2026

### Development build on this host

Run from a configured GHC development worktree with the matching Stage0 toolchain. The following command queues a linked-compiler rebuild in Pueue. The private directory retains parse records when the worker exits.

```sh
install -d -m 700 "$PWD/_build/worker-cpp-cache"
"$HOME/Code/heavy-management/bin/heavy" --mem 8G --time 20m env \
  GHC_WORKER_CPP_CACHE="$PWD/_build/worker-cpp-cache" \
  HADRIAN_SINGLE_CAPABILITY=1 \
  HADRIAN_DEPENDENCY_WORKER="$HOME/Code/ghc-persistent-worker/_build/stage0-bin/current/ghc" \
  hadrian/build -j3 --flavour=quick --docs=none --freeze1 stage2:exe:ghc-bin
```

`--freeze1` reuses the existing Stage0 prerequisites for this benchmark. For a final build, use ordinary GHC and leave the development settings unset.

### Current development workers

Both stages use `c33551f`. On the 956-module dependency scan, a fresh daemon with a populated cache reduced the Stage0 median from 9.98 s to 6.15 s (1.62×) and the Stage1 median from 25.65 s to 11.68 s (2.20×). The worker now recognizes line markers that GHC accepts in noncanonical UTF-8. See the [release and GHC 10 rebuild report](rebuild-c33551f.md) for individual workloads, the correction to the earlier consumer test, linker trials and experiments with reuse inside an optimized binding.

Hadrian now writes each library stamp from one rule. This removed an extra package copy and database refresh that a faster linker exposed. With the fix, one capability and the active scan worker, the median linked compiler edit took 12.12 s with BFD and 9.33 s with mold. All four immediately following unchanged builds ran zero builders. [Matched linked-target measurements](rebuild-c33551f.md#fix-for-the-repeated-package-copy)

An edit to `GHC.Utils.Misc` triggered 305 compiler invocations. GHC skipped code generation for 304 importers. In a separate experiment, routing these requests through the worker reduced the median linked rebuild from 33.44 s to 26.01 s. Both producer object hashes differed. [High-fanout rebuild and artifact checks](high-fanout.md)

### Development workers at fa68e89

A previous diagnostic-free parse can establish that a module has no CPP prerequisites or source line markers. The worker saves this result in a bounded file. After a restart it checks the preprocessed source and parser settings, then asks GHC to resolve imports and emit dependencies. Linux workers verify the executable and file-backed executable mappings before using the file. Replacing a library at the same path invalidated earlier records in the verification fixture.

| GHC 10 dependency command | Stock | Restart with disk cache | In-memory repeat |
| --- | ---: | ---: | ---: |
| Stage0 compiler, 956-module scan, three pairs | 10.88 s | 7.77 s | 0.594 s |
| Stage1 compiler, 956-module scan, two pairs | 26.81 s | 14.99 s | 0.685 s |

The first Stage0 scan to populate the cache took 12.06 s. A fresh Stage0 worker with the disk cache disabled took 10.83 s in one control. On the first edit after restarting it reused 827 of 828 eligible parses. Stage1 took 27.42 s to populate its cache. The Stage1 hot edit took 0.689 s versus 26.42 s stock. All reported dependency files, backups, diagnostics and statuses matched stock. Stage1 used the captured dependency command with that compiler and its libdir.

The two stages need different worker builds on this host. The Stage0 executable links the GHC 10 library built by the bootstrap compiler. Building the daemon with that Stage0 executable normally loads the shared GHC library built for Stage1. The new Stage0 worker links the same GHC 10 API archive as its stock compiler. In the initial comparison, a prefilled restart took 7.92 s with the archive-matched worker and 14.22 s with the dynamic worker. The GHC library and RTS both changed. The Stage1 worker continues to use the shared library.

The full linked-target edit took 45.07 s with default settings in one control. With one Hadrian capability, two order-balanced runs took a median 22.20 s with stock scans and 12.07 s with the new Stage0 worker. These two arms used the same Hadrian configuration. The worker saved 9.61 s and 10.64 s respectively. All 17 artifacts and the optimized consumer matched stock. The worker's disk option was unset during these builds. The savings came from the resident worker. Priming times are reported separately because changing the selected scanner causes additional package rules to run.

On a separate `GHC.Cmm.Pipeline -O2 -c` benchmark, median wall time across three post-warmup rounds was 0.468 s stock, 0.230 s on the static Stage0 worker and 1.141 s on the previous dynamic worker. Median sampled peak RSS was 170, 460 and 551 MiB respectively. Twelve additional one-shot fixtures covered instances, boot imports, rewrite rules and an expected failure. Outputs and diagnostics matched. The scoped Hadrian Stage1 `GHC.Cmm.Pipeline.o` edit took 11.10 s stock and 2.17 s with the worker handling both `-M` and `-c`, a 5.12× speedup across three alternating pairs. All five output files matched, including dynamic objects and interfaces. This was a separate worker-routing experiment. The normal `HADRIAN_DEPENDENCY_WORKER` setting selects only dependency scans. [Release evidence](release-fa68e89.json)

### Linked rebuild with the Hadrian runtime option

We ran the same `GHC.Cmm.Pipeline` edit and linked-compiler target in three rounds. Each configuration occupied the first, second and third position once. All used the same Hadrian executable and stock Shake 0.19.9. Setting `HADRIAN_SINGLE_CAPABILITY=1` changes the RTS running Hadrian. Shake still has three external job slots. The compiler and worker use their own RTS settings.

| Configuration | Median edit to linked compiler | Median no-op |
| --- | ---: | ---: |
| Stock GHC, default Hadrian | 41.26 s | 25.20 s |
| Stock GHC, one Hadrian capability | 19.64 s | 4.78 s |
| Worker for scans, one Hadrian capability | 11.67 s | 4.84 s |

Together the changes gave **3.54×** on the edited build. The Hadrian option alone gave **2.10×** on the edit and **5.27×** on the no-op. Each of the nine edits ran one dependency scan, three ordinary module compilations, one archive action, two link actions and one package recache. All nine no-ops ran zero builder actions. The worker regenerated one of 956 rows on each edit it handled. All modes produced the same 17 artifacts, including both dependency files, both GHC libraries and the linked compiler. The optimized consumer also matched stock and returned 156.

We primed each arm with the original source before timing the edit. Across three rounds the primes took a median 40.33 s stock, 19.79 s with the Hadrian setting alone, and 50.38 s with both changes. Switching scanners caused extra package dependency rules to run in some priming builds. Those builds ran 43 actions, including 36 scans. The others ran eight actions, including one scan. During the first combined prime, three external GHC commands overlapped. Archive and link commands overlapped during its measured edit.

The main comparison used four logical CPUs on two physical cores on WSL2. On no-ops, median system CPU dropped from 13.27 s to 0.57 s with the Hadrian option. A diagnostic build that set the capability count directly in Shake reduced median no-op wall time from 26.34 s to 5.00 s across three pairs. Median GC CPU rose from 0.74 s to 1.33 s. This points to time spent outside GC. With stock Shake and one logical CPU from each of four distinct physical cores, three further no-op pairs took **25.21 s** with default settings and **4.90 s** with one capability (5.15×). Both CPU layouts were on the same machine. [Timings, action counts and topology](hadrian-capabilities.json) · [Hadrian option](../../hadrian/src/Main.hs)

### Thread migration during Hadrian dependency checks

The `-qm` RTS flag disables automatic thread migration. It can be passed directly to the standard Hadrian wrapper. A wrapper smoke used the machine-readable RTS report to verify that Hadrian received the flag.

```sh
hadrian/build -j3 --flavour=quick --docs=none --freeze1 stage2:exe:ghc-bin +RTS -qm -RTS
```

We measured the real `GHC.Cmm.Pipeline` edit in forward and reverse order on the same two physical cores. Hadrian retained all three RTS capabilities and its three external job slots with `-qm`. Stock took a median 39.09 s to link the compiler, 24.89 s with `-qm` alone and 13.95 s with `-qm` and worker `55f502b`. Corresponding no-ops took 26.76, 6.15 and 5.57 s. All six edits ran eight actions and all no-ops ran zero. The 17 artifacts matched and the optimized consumer returned 156.

A separate no-op run used four distinct physical cores. Disabling migration brought median condition waits from 710,420 to about 9,444 and wall time from 26.09 to 5.03 s. A native probe of the default reported 1,370 of 1,380 sampled signals along the path from `stg_putMVarzh` through `sendMessage` to `giveCapabilityToTask`. All 1,376 sampled waits came from `yieldCapability` in scheduler workers. This connects cross-capability wakeups to the slowdown on this workload. Hadrian used the GHC 9.14.1 bootstrap RTS. The comparison used the same GHC 10 targets throughout. [Timing samples, pthread counts and stacks](thread-migration.json) · [Measured RTS scheduler](https://github.com/ghc/ghc/blob/902339d332fb4ce2b3c87dcac1ee6495d41ad886/rts/Schedule.c)

### Research priorities

1. **Extend restart reuse to more modules.** Of the 108 modules that still required parsing, 104 produced preprocessed buffers with different temporary paths after restart. Their parses and the four other parses cost about 1.3 s combined. Measure validation overhead before extending cache eligibility. [Cold dependency scans](#cold-dependency-scans-after-worker-restart)
2. **Validate reuse inside an optimized Core binding.** Recording transitive rule heads detected a change behind an inlined wrapper. Four local lookups from the first visit to `doesDirNameExist` could not be validated at binding entry. Account for those local dependencies, substitution and floated bindings. Compare every proposed reuse with a fresh optimization. [Core measurements](#where-optimization-time-survives-a-small-edit)
3. **Measure thread migration on other builds.** The no-op traced most condition signals to cross-capability MVar wakeups. Repeat on another machine and at a wider job count before considering a default. The compiler worker and the GHC executable have different allocation and parallelism profiles. [Compiler RTS measurements](#rts-cost-during-compilation)

### Hadrian dependency caching

Set `HADRIAN_DEPENDENCY_WORKER` to the absolute path of a prepared worker's `ghc` executable for development builds. The retained stage0 compiler on this host uses the following setting.

```sh
export HADRIAN_DEPENDENCY_WORKER="$HOME/Code/ghc-persistent-worker/_build/stage0-bin/current/ghc"
```

Run the usual Hadrian target through Pueue. Hadrian selects the worker for Haskell dependency scans when its recorded compiler path and executable hash match the stage compiler. Other stages use their own compiler. Leave the variable unset for final builds. The digest covers the executable. Prepare a new worker after separately replacing shared libraries.

Worker `21fb2cf` recognizes Hadrian's `+RTS -O64M -RTS` option and starts a separate daemon with that setting. Its predecessor forwarded this command to stock. Using the exact stage0 compiler for the Stage1 compiler package, a comment edit took 14.52 s stock and 2.75 s warm for the `.dependencies.mk` rule. It regenerated one of 956 modules and emitted byte-identical output (5,244,968 bytes).

For the `--flavour=quick` (`-O0`) `GHC.Cmm.Pipeline.o` target, the same type of edit took 14.74 s stock and 2.97 s warm (4.96×). Each arm ran one `-M` and one `-c`. The worker handled the scan, and the ordinary compiler rebuilt the object. The edited interface changed from the primed one. The Makefile, static and dynamic objects, and both interfaces matched between arms. No-op times were 2.23 s and 1.69 s, with zero compiler invocations.

For an optimized rebuild we changed a string on the right-hand side of `cpsTop`, from `Post CPS Cmm` to `Post CPS Cmm pass`. Hadrian appended `stage1.ghc.ghc.hs.opts+=-O2` to the quick flavour. The last optimization flag in each compilation command was `-O2`. For each arm we restored the same pre-edit Shake database and optimized objects. With release `21fb2cf`, across five pairs, stock took a median **16.38 s** and the worker **3.06 s**, a 5.35× ratio of medians (4.97× median paired speedup). No-ops took 1.83 s stock and 1.79 s with the worker. The edited object differed from the primed one. The Makefile, both objects and both interfaces matched stock in all five pairs. Each edit used one dependency scan and one ordinary GHC compilation. The benefit here came from dependency discovery. [Individual edit pairs and hashes](pipeline-o2.json)

The dependency-only and `-O0` measurements above are single pairs on a shared host. All three experiments retained prerequisites outside the target. A complete self-build also runs other package rules and linking.

### Whole compiler target

Hadrian built `stage2:exe:ghc-bin` with `--flavour=quick --freeze1 -j3` in a private copy of GHC 10. It ran the ordinary Stage1 prerequisite and link rules. The stock warm-up after relocating the tree took 405.51 s. We then added `&& splitting_proc_points` to the `cmmOptElimCommonBlks cfg` guard in `GHC.Cmm.Pipeline`. Each measured edit rebuilt the module and two direct importers, the static and dynamic GHC libraries, and the linked compiler.

| Edit with worker 21fb2cf or its stock compiler | Wall time | Dependency scan |
| --- | ---: | --- |
| Stock | 48.70 s | Ordinary GHC |
| Worker after scanning several packages | 59.61 s | Full, 956 modules |
| Worker with the compiler graph resident | 32.97 s | Partial, 1/956 |

These are single observations. The warm scan reduced end-to-end time by 32.3%. The first worker edit took 22.4% longer than stock. Another package scan can evict the compiler graph from this release's two entries. The full-target trace shows the miss. It does not record the precise eviction or argument change that caused it. Both worker builds matched the stock compiler executable, measured module artifacts and dependency file byte for byte. The resulting compilers built the same optimized test executable, which returned 156.

The no-op took 31.74 s stock and 26.26 s with the warm worker. Neither ran a builder action. A separate Shake timing run at `-j3` put 30.44 of 31.82 s in rule evaluation. The process allocated 11.63 GB and spent 1.24 CPU seconds in GC. One `-j1` no-op completed in 6.12 s. Shake 0.19.9 also changes the number of RTS capabilities with the job count. That experiment varies both at once.

Release `55f502b` retains the two recent scans and can retain one older graph larger than those two combined. This keeps the compiler graph after small package scans. It also tracks the appearance and disappearance of files named by raw, Haskell or CPP-generated source line markers.

We repeated the full-target edit using the revised release. Stock took **42.46 s** and the worker **32.28 s** (1.32×, 24.0% less elapsed time). The worker regenerated 1 of 956 modules on the first measured edit after its cold package scans. Stock and worker no-ops took 26.66 and 25.90 s. Both ran zero builder actions. The priming builds took 46.70 s stock and 62.88 s worker. The scope was the linked GHC executable and its normal Stage1 prerequisites. There was one edit pair, so the timings include host variation.

The linked compiler, both GHC libraries and the static and dynamic object and interface files for all three modules matched stock. The compiler built an optimized consumer whose interface, object and executable also matched. The consumer returned 156. [Whole-target timings and hashes](whole-55f502b.json)

With the same release, we repeated the optimized `GHC.Cmm.Pipeline.o` target experiment. The five alternating pairs took a median **11.86 s stock and 2.49 s warm** (4.76× by medians, 4.86× median paired). Every edit ran one dependency scan and one stock compilation. All five output files matched in all ten runs. The corresponding no-ops took 1.46 s and 1.39 s and ran no compiler. Stock times also changed between releases. Use the matched stock result for each comparison. [Optimized object measurements](pipeline-o2-55f502b.json)

The separate Stage1 worker served the same 956-module compiler dependency graph in a direct command benchmark. After two intervening small scans, one import edit took 26.912 s stock and 0.713 s warm (three-pair medians, 37.7×). The cold times were 27.088 s stock and 26.293 s worker. The warm daemon reached 749 MiB sampled RSS. Both promoted paths also passed response-file, tuned RTS and line-marker checks. Creating or removing a line-marker target refreshed one of three modules. An unchanged repeat reused all three. The Makefile, backup and diagnostics matched stock.

### Cold dependency scans after worker restart

We profiled the Stage1 compiler package's 956-module `-M -include-cpp-deps` command with its stock Stage0 GHC. Dependency discovery allocated 2.64 GB and took 1.40 CPU seconds. The 956 parser calls allocated 15.24 GB and took 9.18 CPU seconds in total. Building the Makefile requires GHC to collect the extra source files that CPP and source line markers name. The uninstrumented process allocated 24.06 GB and took 15.95 CPU seconds. These are compiler-process measurements. Clang implements a separate scanner for directives that affect include dependencies. GHC also reports parse errors through this path. Reusing a previous successful parse requires preserving those diagnostics after an edit. [Clang directive scanner](https://github.com/llvm/llvm-project/blob/main/clang/include/clang/Lex/DependencyDirectivesScanner.h) · [Dependency emitter](../../compiler/GHC/Driver/MakeFile.hs) · [Cold-scan phase totals](cold-scan.json)

Two fresh Stage0 processes produced identical preprocessed-input fingerprints for **852 of 956 modules** with unchanged source. After adding `import GHC.Data.Bool ()` to `GHC.Cmm.Pipeline`, 851 matched. Dependency discovery took 3.52–3.53 s per process. Hashing all 32.6 MB took 37–39 ms. All 104 unchanged-source misses involved CPP output. We compared two 81,552-byte `GHC.hs` buffers directly. Only three PID digits in the filename of a generated header in a CPP line marker differed. The other 103 require separate investigation. This measures the cost and stability of one input to the cache key. Import resolution, diagnostics and output parity also need validation. [Fresh-process measurements](preprocessed-fingerprints.json)

An extension under consideration is to store complete per-module dependency rows. Release `fa68e89` stores the successful empty CPP parse result. Start a new daemon, run GHC's dependency discovery and check the discovered edges before reading saved rows. Key the cache on the worker format, exact compiler and libdir, package environment, working directory and arguments. Fingerprint the actual preprocessed Haskell input (`ms_hspp_buf` or `ms_hspp_file`). GHC's `ms_hs_hash` identifies the source before preprocessing. Resolve each import with the fresh Finder, including the selected external interface path. Two installed package layouts can select different prerequisites for the same module name. Check the existence of all recorded line-marker targets, including paths that did not exist. Restart with empty GHC session state. Regenerate rows that produced diagnostics. Initially record which rows the disk cache would have returned and continue to run the ordinary emitter so every predicted hit has a stock comparison. Verify the resulting bytes and diagnostics against stock after restarting the daemon and after changing each input. Include a generated module that begins to shadow an existing import. [Source hashing in the downsweep](../../compiler/GHC/Driver/Downsweep.hs)

### Reuse inside an edited module

GHC 10 checks the source fingerprint when deciding whether to recompile a module. For an importer it can also check fingerprints of individual declarations it used. This already avoids downstream work. We measured what remains identical inside a module selected for recompilation. [Recompilation checker](../../compiler/GHC/Iface/Recomp.hs)

We changed one condition in the private function `cpsTop` in `GHC.Cmm.Pipeline`. The edit required `splitting_proc_points` for common-block elimination. We compiled both sources at the same path using the same dependencies and `-O2 -fforce-recomp -split-sections`. The compiler was GHC 10.1.20261115.1, source revision `945ea7a737`, on x86-64 WSL2.

With `-fobject-determinism`, 52 of 57 named text sections retained their bytes and GNU relocation records. They held 5,709 of 12,024 original text bytes (47.5%). One section retained its bytes but changed relocations. With default naming, zero sections matched all three criteria. Two stock builds of the original matched in full. With deterministic naming, worker `39af263` matched stock on both versions. [Deterministic Cmm names](../../compiler/GHC/Cmm/UniqueRenamer.hs)

Before Cmm optimization, 55 of 56 nonempty groups had identical printed contents. They held 31,765 of 121,347 printed bytes (26.2%). We removed only dump headings, timestamps and surrounding line breaks. Repeated stock builds of the original and warm worker builds of both sources reproduced the corresponding dumps, objects and interfaces.

The stock compiler's eventlog identified 59 timed Cmm pipeline calls. Three received empty groups. In two runs of each source, the 55 matching nonempty groups allocated exactly 7,638,912 bytes. This was 21.3% of the original Cmm pipeline allocation and 3.6% of total code-generation allocation. Their measured spans summed to 4.1–5.4 ms on the original. The single changed group took 78.5% of Cmm pipeline allocation. These timings cap the gross opportunity for skipping these calls on this edit. Cache lookup and validation still take time.

A candidate reuse boundary is `cpsTop`, before the module-wide static reference table pass. Its result includes CAF information and an updated unique supply. A cache must validate or reproduce both, as well as platform settings and optimization flags. Rust's query system tracks dependencies and compares result fingerprints across builds. GHC already supports writing simplified Core to disk with `-fwrite-if-simplified-core`. It stores the result after optimization. To target repeated optimizer work, measure stable groups and phase costs before and during simplification. Validate cross-binding inlining, specialization and rule dependencies against a stock build. [Interface construction](../../compiler/GHC/Iface/Make.hs) · [Cmm pipeline](../../compiler/GHC/Cmm/Pipeline.hs) · [Rust incremental compilation](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html)

### Where optimization time survives a small edit

In `GHC.Utils.Misc` we changed the edit-distance helper's `Word32` cutoff from 32 to 31. We compiled both versions with stock GHC 10 at `-O2`. Two repeats of the original gave identical desugared and final Core dumps after removing dump headers and timestamps. We compared complete top-level groups as multisets, retaining printed uniques, types, coercions, binder metadata, recursive groups and right-hand sides.

Of 153 desugared groups, 152 matched after the edit. Of 309 final groups, 308 matched. The only textual difference in each unmatched group was the changed literal. The 1,142 rule-firing records and the printed local rules also matched. Repeated object and interface files agreed within each source version.

The ten simplifier invocations took 3.39–3.69 CPU seconds and allocated 5.405 GB per baseline run. The first invocation alone took 566 ms and allocated 986 MB in the first run. Those are whole-pass costs. To establish the value of a cache, attribute the cost to candidate groups, then record which unfoldings, rewrite rules and other binder information the simplifier actually reads. An unchanged result can depend on a changed optimization input. LLVM ThinLTO includes imported functions and global analysis decisions in its incremental cache key. GHC would need a corresponding account of its own dependencies. [Simplifier iteration](../../compiler/GHC/Core/Opt/Simplify/Iteration.hs) · [ThinLTO design](https://blog.llvm.org/2016/06/thinlto-scalable-and-incremental-lto.html)

We then instrumented the first simplifier invocation in a separate Stage0 GHC 10 compiler. The stock and uninstrumented relink had identical pass allocation (180.97 MB). The probe added 4.1% allocation. Across four iterations it attributed 158.27 MB to binding groups. Groups with no observed Core change accounted for 150.25 MB (94.9% of attributed allocation). The edited helper itself accounted for 1,024 bytes in iteration one, but the literal also reached `fuzzyLookup`, which accounted for 4.67 MB. The largest unchanged group in that iteration, `doesDirNameExist`, accounted for 15.52 MB. These measurements apply to the Stage0 compiler. The Stage1 timings above use a different build of GHC.

Start by recording successful and failed unfolding and rewrite-rule lookups for one expensive, unchanged nonrecursive binding. Compare the complete result with ordinary simplification on every run. Track floated bindings, substitution, scope, fresh names and simplifier ticks before attempting to skip work. Identical printed output across one edit alone cannot establish cache validity.

### Which simplifier inputs determine an optimized binding?

A private GHC 10 Stage0 compiler traced rule lookups and inlining attempts while simplifying `GHC.Utils.Misc.doesDirNameExist` at `-O2`. Its 794 recorded decisions matched in order across two original runs and an unrelated edit to the edit-distance cutoff. The trace contained 397 empty rule lookups, 395 rejected inline attempts and two accepted attempts. It covered 71 visits to a binding with that name over the optimization pipeline. Objects and interfaces from the instrumented compiler matched stock for every tested version.

A controlled edit shows why replaying just those decisions can produce stale code. We gave the binding an `INLINE` helper and changed that helper from `take 13` to `take 14`. The caller's source stayed the same. All **982 recorded decisions** in the caller matched, but its optimized worker called `$wunsafeTake` with `13#` in one build and `14#` in the other. This immediate was the only difference in the final Core dump after removing the header.

Adding a fingerprint for each expression selected for inlining detected the changed helper. Of six accepted inlining attempts, exactly the helper's fingerprint differed. These diagnostic fingerprints use GHC's printed Core. A reusable cache key still requires a stable encoding, the other inputs to each transformation, and validation against stock results. This experiment measured dependencies and did not skip optimization work. [Probe results and output hashes](simplifier-inputs.json) · [Simplifier](../../compiler/GHC/Core/Opt/Simplify/Iteration.hs)

### Cmm assignment sinking

Research commit `96cbe8010a` skips conflict checks for a live destination with a nontrivial right-hand side. All outcomes of those checks retain the assignment. The candidate stays on the research branch.

| Forced module at `-O2` | Baseline wall | Candidate wall | Baseline allocation | Candidate allocation |
| --- | ---: | ---: | ---: | ---: |
| Generated 200-field applicative | 0.7140 s | 0.6654 s | 1,704 MB | 1,519 MB |
| GHC.Cmm.Sink | 0.6023 s | 0.6167 s | 530.4 MB | 530.0 MB |
| GHC.Utils.Misc | 1.9518 s | 1.7728 s | 1,997.5 MB | 1,995.7 MB |
| GHC.Data.Bool | 0.2133 s | 0.2254 s | 163.6 MB | 163.6 MB |

The table gives medians from five interleaved runs after one warm-up. We relinked both GHC 10 executables from the same retained package and main objects, changing only `GHC.Cmm.Sink`. This package used the GHC 9.14.1 bootstrap RTS. The sinking source matched the research base `56bb91d95f`. All measured pairs had identical object and interface files. A separate GHC API driver at the research base used 6.107 GB and 1.633 CPU seconds on the 400-field case, down from 7.749 GB and 1.905 seconds. The GHC modules give mixed elapsed times.

We ran a 200-field consumer with stateful IO actions against each producer. The worker compiled the consumer and stock GHC linked both executables. The executables had identical SHA-256 hashes. With seeds 7 and 41, both returned the expected sums, position-weighted sums and counters.

A separate shortcut in Core simplifier rule lookup reduced total compiler allocation by 0.31% on `GHC.Utils.Misc` and 0.27% on `GHC.Data.FastString`. Median CPU times stayed within 1 ms of the controls. We discarded that candidate.

A separate Stage1 experiment compiled only `GHC.Core.Opt.Simplify.Iteration` with `-O2` inside the otherwise quick compiler. Against matched static relinks using the original Stage1 interfaces, the median paired workload savings across four runs were 195 ms for `GHC.Utils.Misc` and 36 ms for `GHC.Data.Bool`. The objects and interfaces matched dynamic stock GHC. Simplifier allocation fell by about 0.7% in both cases. Rebuilding `Iteration` itself took an additional 1.14 s and 53 MiB peak RSS. We kept this as a local experiment because each workload included one nearly flat timing pair.

### RTS cost during compilation

We ran the retained GHC 10.1.20261115.1 compiler with its own threaded dynamic RTS. Four concurrent processes used four physical cores in one Pueue job with a 2 GiB memory cap and swap disabled. Each process compiled a separate copy of its target. GC CPU is summed across the four processes. Peak memory is sampled for their cgroup.

| Four-process workload | Rounds per setting | Median wall, default / 64 MiB nursery | GC CPU, default / 64 MiB | Peak memory, default / 64 MiB |
| --- | ---: | ---: | ---: | ---: |
| Two GHC.Cmm.Sink and two GHC.Data.Bool | 6 | 2.161 / 2.049 s | 1.521 / 1.044 s | 254 / 470 MiB |
| Four GHC.Cmm.Sink | 4 | 2.299 / 2.162 s | 2.148 / 1.586 s | 427 / 636 MiB |

The 64 MiB nursery cut median wall time by 5–6%. Timing ranges overlapped. All objects and interfaces matched. The extra 209–216 MiB matters when several builds share a machine. We kept the existing defaults. The [individual samples](2026-11-27.csv) include smaller nursery settings.

In a separate Callgrind run, sequential evacuation in `evacuate1` used 474,394,570 of 3,386,978,025 instructions (14.0%) in the compiler process building `GHC.Data.Bool`. Both artifacts matched the uninstrumented build. Of 8,694,351 pointer classifications, 4,618,238 took the static path. Of 2,540,069 ordinary closure dispatches across 16 targets, the largest target received 743,727. Removing the suspected extra flag load would save at most 0.12% of the routine's instructions. This profile gives little support for specializing one closure type. The `GHC.Cmm.Sink` profile reached its 100 s limit without usable instruction counts. [Evacuation code](../../rts/sm/Evac.c)


### Capability count in the worker daemon

A separate test sent four simultaneous compilation requests to one warmed `21fb2cf` Stage0 worker on four physical cores. Each request compiled a different GHC module. With the daemon's default `-N`, the median wall time to finish all four was 9.00 s. Forcing `-N1` took 17.99 s, and `-N2` took 13.84 s. Median sampled daemon RSS peaks were 703, 534 and 625 MiB respectively. All 56 completed module compilations in the recorded trials produced the stock artifacts. The timings use the two complete rounds. A daemon expired during the third. We retained the worker's capability default. Hadrian and the compiler worker run in separate processes. [Worker capability samples](worker-capabilities.json) · [GHC runtime options](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/runtime_control.html)
