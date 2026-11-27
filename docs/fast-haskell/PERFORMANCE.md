# Incremental GHC self-builds

Use ordinary GHC for final builds. At each development-worker switch, compare with the exact GHC executable behind that worker. At each GHC or RTS switch, use an unmodified build of the same source revision, flavour and toolchain. Record revisions, workload, sample count, results and the reason for the switch. Include memory for changes that retain data or change the heap. Each worker release keeps its own `PERFORMANCE.md`.

## 27 November 2026

### Published importer rule on GHC 10.1.20261127

The original Hadrian `56bb91d95f` used ordinary GHC. The optimized build used the published Hadrian rule, one Hadrian capability, and a warm `e0d203b` worker for dependency scans and compile checks. Both used the same Stage0 executable (SHA-256 prefix `cafe7121a8e6`), `perf+no_profiled_libs --freeze1 -j3`, and logical CPUs 20–23. Both targeted `stage2:exe:ghc-bin` and used the default compiler nursery.

| Linked `-O2` rebuild | Original Hadrian | Published importer rule |
| --- | ---: | ---: |
| Change cutoff 32 to 31 | 76.75 s | 25.21 s (3.04×) |
| Immediate repeat | 33.37 s | 4.14 s |
| Revert 31 to 32 | 75.68 s | 27.91 s (2.71×) |
| Immediate repeat | 25.57 s | 5.05 s |

The edit and revert together took **152.43 s with the original build and 53.12 s with the optimized build (2.87×, 65.1% less time)**. The optimized build issued 54 compile requests per edit instead of 303 and regenerated one of 943 dependency rows. Only the edited producer generated code. The checked module artifacts, package archive, shared library, linked compiler and separately compiled dynamic API consumer matched ordinary GHC. Every immediate repeat ran zero build actions.

Separate writable overlays used the same read-only build. Preparation and worker warm-up took place before each stopwatch interval. Each cell is one sample. After correcting the primer, the original windows ran before the optimized windows. Pueue recorded overlap with a small-group task in the original forward window and a build-group task in the optimized reverse window. Whole-job memory peaks were 2,548 and 2,491 MiB for the original windows and 3,425 and 3,504 MiB for the optimized windows, including worker warm-up and verification.

### Cumulative comparison on GHC 10.1.20261127

We built Hadrian from the branch point `56bb91d95f` and used ordinary GHC for both dependency scanning and compilation. The development configuration used the published Hadrian changes, one Hadrian capability, and a stage-matched worker for dependency scans and compile checks with its decoded-interface cache enabled. The pilot also used an importer's actual source options and interface usage records to decide whether a change to `GHC.Utils.Misc` required a compiler call. Hadrian still passed changed code to the ordinary compiler and relinked its library and executable.

| Linked `-O2` rebuild | Starting Hadrian + ordinary GHC | Published development options | Automatic importer pilot |
| --- | ---: | ---: | ---: |
| Change cutoff 32 to 31 | 63.46 s | 30.65 s (2.07×) | 26.36 s (2.41×) |
| Revert 31 to 32 | 59.70 s | 33.85 s (1.76×) | 25.90 s (2.31×) |
| No-op after forward edit | 23.54 s | 4.00 s | 3.82 s |
| No-op after revert | 23.95 s | 4.43 s | 3.85 s |

Across the edit and revert together, the published options cut wall time by 47.6% (1.91×). The pilot cut it by 57.6% (2.36×).

Each cell is one run on the same previously built source tree, native Stage0 compiler, four CPUs, `-j3`, and `perf+no_profiled_libs --freeze1`. All arms ran the normal `stage2:exe:ghc-bin` target. The starting compiler executable was held constant so these comparisons measure the build system and worker. The development worker was warm. It regenerated one of 943 dependency rows on each edit. The starting configuration ran an ordinary dependency scan. The development and pilot windows ran before the branch-point control, so the ratios include run-to-run noise.

The starting and published configurations ran 303 Haskell compile commands. The pilot ran 54. Only the edited producer generated code. The module artifacts of all 249 skipped importers, the compiler archive, shared library, compiler executable, and a separately compiled and executed GHC API consumer matched the ordinary reference. Each immediate no-op ran zero builder actions. The two separate starting benchmark jobs peaked at 2,564 and 2,578 MiB in their cgroups. The two-direction worker and pilot jobs peaked at 5,772 and 4,680 MiB. These are whole-job peaks, including the compiler and resident workers.

### Development build on this host

Run from a configured GHC development worktree. Set `WORKER_RELEASE` to the prepared worker for that worktree's Stage0 compiler. The worker's `ghc.real-ghc` must resolve to that compiler, and its `ghc.libdir` must exist. The following command queues a linked-compiler rebuild in Pueue. The private directory retains parse records when the worker exits.

```sh
install -d -m 700 "$PWD/_build/worker-cpp-cache"
"$HOME/Code/heavy-management/bin/heavy" --mem 8G --time 20m env \
  GHC_WORKER_CPP_CACHE="$PWD/_build/worker-cpp-cache" \
  HADRIAN_SINGLE_CAPABILITY=1 \
  HADRIAN_DEPENDENCY_WORKER="$WORKER_RELEASE/ghc" \
  hadrian/build -j3 --flavour=quick --docs=none --freeze1 stage2:exe:ghc-bin
```

`--freeze1` reuses the existing Stage0 prerequisites for this benchmark. For a final build, use ordinary GHC and leave the development settings unset.

### Shared Stage1 worker on 27 November

The active shared Stage1 worker `fd8d51d` uses GHC 10.1.20261115.1 (executable SHA-256 prefix `aac58b24`). It ran the 956-module dependency command in 0.577 s unchanged and 0.617 s after a one-import edit. The old worker `2a5a1d0` took 0.589 and 0.608 s in the same test. These are medians from six warm samples per worker per case. Ordinary GHC took 23.284 and 23.498 s in one reference run per case. The edit command was 38.1× faster and regenerated one row. All Makefiles, backups and diagnostics matched ordinary GHC.

The release also handles an up-to-date `-c -dynamic-too` command with Hadrian's optional `+RTS -O64M -A64m -RTS` block. Across six warm samples per executable on a single-file fixture, ordinary GHC took 210.1 ms, the previous worker took 209.4 ms, and the new worker took 19.6 ms at the median (10.7× versus ordinary GHC). A required compile delegated once with the original arguments and reproduced all four artifacts byte for byte.

### Shared Stage0 worker on 27 November

The shared Stage0 selector now uses `df96e32` and a complete GHC 10.1.20261127 toolchain on a durable path (executable SHA-256 prefix `cafe7121a8e6`). The ordinary 943-module dependency command took 9.527 and 10.041 s in the two reference source states. The warm worker took 0.436 s unchanged and 0.476 s after a one-import edit (20.6× versus the stock reference median). Both worker medians use six samples. Each edit regenerated one row. Makefiles, backups and diagnostics matched. A warm tuned `-c -dynamic-too` check on a single-file fixture took 6.1 ms with the worker and 21.6 ms with ordinary GHC across six alternating pairs. A required compile delegated once with the original arguments and reproduced all four outputs.

The previous shared Stage0 worker pointed to a different installation with a missing Stage0 library directory. Hadrian enables the worker only when it targets the exact compiler path selected for that stage. The linked results above used the same GHC binary bytes in a separate installation.

### Remaining compile requests

The normalized linked edit issued 53 up-to-date compile requests. Forty-eight used CPP and five were boot modules. A private default-CPP check replayed the 48 commands in four paired batches of three clients. The median fell from 3.30 s with ordinary GHC to 2.03 s with the worker. Diagnostics matched. One request delegated in one timed batch. The prototype ran default CPP again through ordinary GHC on an edit that required compilation, so we kept it private.

The changed `GHC.Utils.Misc` producer took 1.40–1.41 s wall in two replays of its actual command. The simplifier accounted for 0.56 s CPU, code generation for 0.34 s, and both interface writes for 0.24 ms. Four output files matched the archived stock hashes. These are compiler-command timings. The linked build also packs the compiler archive, links the shared library and executable, and registers the package.

### Development workers at 8ad5a4d

Both stages use `8ad5a4d`. A warm edit of one import in a 956-module graph took **0.582 s versus 10.438 s stock at Stage0 (17.9×)** and **0.619 s versus 23.379 s stock at Stage1 (37.8×)**. The worker reused rendered dependency rows when their paths matched. The release also fixed stale single-file compilation results after package and environment changes. [Raw scan samples](release-8ad5a4d.json)

| 956-module scan | Stage0 stock | Stage0 worker | Stage1 stock | Stage1 worker |
| --- | ---: | ---: | ---: | ---: |
| Warm repeat | 10.232 s | 0.566 s | — | 0.597 s |
| Empty disk cache, first scan | 11.845 s | 10.638 s | — | 23.174 s |
| Restart with populated disk cache | 11.845 s | 6.586 s | 23.586 s | 10.929 s |

Warm Stage0 timings are medians from three balanced comparisons. The prior worker took 0.648 s on a warm repeat and 0.583 s on the edit. Stage0 median sampled peak RSS was approximately 576 MiB for the candidate and 294 MiB for stock. Stage0 cold fill and restart each used one sample per worker, between two stock controls. Stage1 restart and edit used two order-balanced pairs. Restarts reused all 848 eligible records at both stages. Makefiles, backups and diagnostics matched stock.

A linked compiler edit in `GHC.Cmm.Pipeline` took 41.26 s with stock defaults, 19.64 s with one Hadrian capability, and 11.67 s when a warm worker also handled the dependency scan. Those three-run medians used an earlier release. A separate experiment gave each library stamp one writer and measured BFD and mold at 12.12 and 9.33 s on the same edit. [Linked-target comparison](#linked-rebuild-with-the-hadrian-runtime-option) · [Library stamp and linker experiments](rebuild-c33551f.md)

A `GHC.Utils.Misc` edit triggered 305 compiler invocations. In a private trial the worker completed 253 of GHC's recompilation checks and delegated the remaining requests to ordinary GHC. The median linked rebuild took 31.70 s versus 34.90 s with ordinary compilation throughout. The compiler, libraries and measured module artifacts matched byte for byte in both source states. [High-fanout timings and artifact checks](high-fanout.md)

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
export HADRIAN_DEPENDENCY_WORKER="$WORKER_RELEASE/ghc"
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

On the actual Stage1 `-O2 -dynamic-too` command for `GHC.Core.Opt.Simplify.Iteration`, the 64 MiB nursery saved 0.75 s of CPU at the median across three paired rounds. Median process peak RSS rose from 349 MiB to 369 MiB. All four emitted artifacts matched across the runs. Other builds were active, so these samples measure per-command CPU and RSS. The nursery has its own development opt-in. [Hadrian setup](../../hadrian/README.md#compiler-nursery-for-development-builds)

Across four paired samples per module, the same nursery setting saved a median 0.10 s CPU when emitting `GHC.Utils.Misc` and took a median 0.015 s more CPU when emitting `GHC.Tc.Gen.Expr`. Peak process RSS rose from 195 to 221 MiB and from 428 to 431 MiB, respectively. All four output files for each command matched ordinary GHC. This panel measured individual compiler processes.

In a separate Callgrind run, sequential evacuation in `evacuate1` used 474,394,570 of 3,386,978,025 instructions (14.0%) in the compiler process building `GHC.Data.Bool`. Both artifacts matched the uninstrumented build. Of 8,694,351 pointer classifications, 4,618,238 took the static path. Of 2,540,069 ordinary closure dispatches across 16 targets, the largest target received 743,727. Removing the suspected extra flag load would save at most 0.12% of the routine's instructions. This profile gives little support for specializing one closure type. The `GHC.Cmm.Sink` profile reached its 100 s limit without usable instruction counts. [Evacuation code](../../rts/sm/Evac.c)


### Capability count in the worker daemon

A separate test sent four simultaneous compilation requests to one warmed `21fb2cf` Stage0 worker on four physical cores. Each request compiled a different GHC module. With the daemon's default `-N`, the median wall time to finish all four was 9.00 s. Forcing `-N1` took 17.99 s, and `-N2` took 13.84 s. Median sampled daemon RSS peaks were 703, 534 and 625 MiB respectively. All 56 completed module compilations in the recorded trials produced the stock artifacts. The timings use the two complete rounds. A daemon expired during the third. We retained the worker's capability default. Hadrian and the compiler worker run in separate processes. [Worker capability samples](worker-capabilities.json) · [GHC runtime options](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/runtime_control.html)
