# Incremental GHC self-builds

Use ordinary GHC for final builds. At each development-worker switch, compare with the exact GHC executable behind that worker. At each GHC or RTS switch, use an unmodified build of the same source revision, flavour and toolchain. Record revisions, workload, sample count, results and the reason for the switch. Include memory for changes that retain data or change the heap. Each worker release keeps its own `PERFORMANCE.md`.

## 27 November 2026

### Hadrian dependency caching

Set `HADRIAN_DEPENDENCY_WORKER` to the absolute path of a prepared worker's `ghc` executable for development builds. The retained stage0 compiler on this host uses the following setting.

```sh
export HADRIAN_DEPENDENCY_WORKER="$HOME/Code/ghc-persistent-worker/_build/stage0-bin/current/ghc"
```

Run the usual Hadrian target through Pueue. Hadrian selects the worker for Haskell dependency scans when its recorded compiler path and executable hash match the stage compiler. Other stages use their own compiler. Leave the variable unset for final builds. The digest covers the executable. Prepare a new worker after separately replacing shared libraries.

Worker `21fb2cf` recognizes Hadrian's `+RTS -O64M -RTS` option and starts a separate daemon with that setting. Its predecessor forwarded this command to stock. Using the exact stage0 compiler for the Stage1 compiler package, a comment edit took 14.52 s stock and 2.75 s warm for the `.dependencies.mk` rule. It regenerated one of 956 modules and emitted byte-identical output (5,244,968 bytes).

For the `--flavour=quick` (`-O0`) `GHC.Cmm.Pipeline.o` target, the same type of edit took 14.74 s stock and 2.97 s warm (4.96×). Each arm ran one `-M` and one `-c`. The worker handled the scan, and the ordinary compiler rebuilt the object. The edited interface changed from the primed one. The Makefile, static and dynamic objects, and both interfaces matched between arms. No-op times were 2.23 s and 1.69 s, with zero compiler invocations.

For an optimized rebuild we changed a string on the right-hand side of `cpsTop`, from `Post CPS Cmm` to `Post CPS Cmm pass`. Hadrian appended `stage1.ghc.ghc.hs.opts+=-O2` to the quick flavour. Both actual compilation commands ended with `-O2`. For each arm we restored the same pre-edit Shake database and optimized objects. Across five pairs, stock took a median **16.38 s** and the worker **3.06 s**, a 5.35× ratio of medians (4.97× median paired speedup). No-ops took 1.83 s stock and 1.79 s with the worker. The edited object differed from the primed one. The Makefile, both objects and both interfaces matched stock in all five pairs. Each edit used one dependency scan and one ordinary GHC compilation. The benefit here came from dependency discovery. [Individual edit pairs and hashes](pipeline-o2.json)

The dependency-only and `-O0` measurements above are single pairs on a shared host. All three experiments retained prerequisites outside the target. A complete self-build also runs other package rules and linking.

### Reuse inside an edited module

GHC 10 checks the source fingerprint when deciding whether to recompile a module. For an importer it can also check fingerprints of individual declarations it used. This already avoids downstream work. We measured what remains identical inside a module selected for recompilation. [Recompilation checker](../../compiler/GHC/Iface/Recomp.hs)

We changed one condition in the private function `cpsTop` in `GHC.Cmm.Pipeline`. The edit required `splitting_proc_points` for common-block elimination. We compiled both sources at the same path using the same dependencies and `-O2 -fforce-recomp -split-sections`. The compiler was GHC 10.1.20261115.1, source revision `945ea7a737`, on x86-64 WSL2.

With `-fobject-determinism`, 52 of 57 named text sections retained their bytes and GNU relocation records. They held 5,709 of 12,024 original text bytes (47.5%). One section retained its bytes but changed relocations. With default naming, zero sections matched all three criteria. Two stock builds of the original matched in full. With deterministic naming, worker `39af263` matched stock on both versions. [Deterministic Cmm names](../../compiler/GHC/Cmm/UniqueRenamer.hs)

Before Cmm optimization, 55 of 56 nonempty groups had identical printed contents. They held 31,765 of 121,347 printed bytes (26.2%). We removed only dump headings, timestamps and surrounding line breaks. Repeated stock builds of the original and warm worker builds of both sources reproduced the corresponding dumps, objects and interfaces.

The stock compiler's eventlog identified 59 timed Cmm pipeline calls. Three received empty groups. In two runs of each source, the 55 matching nonempty groups allocated exactly 7,638,912 bytes. This was 21.3% of the original Cmm pipeline allocation and 3.6% of total code-generation allocation. Their measured spans summed to 4.1–5.4 ms on the original. The single changed group took 78.5% of Cmm pipeline allocation. These timings cap the gross opportunity for skipping these calls on this edit. Cache lookup and validation still take time.

A candidate reuse boundary is `cpsTop`, before the module-wide static reference table pass. Its result includes CAF information and an updated unique supply. A cache must validate or reproduce both, as well as platform settings and optimization flags. Rust's query system tracks dependencies and compares result fingerprints across builds. GHC already supports writing simplified Core to disk with `-fwrite-if-simplified-core`. It stores the result after optimization. To target repeated optimizer work, measure stable groups and phase costs before and during simplification. Validate cross-binding inlining, specialization and rule dependencies against a stock build. [Interface construction](../../compiler/GHC/Iface/Make.hs) [Cmm pipeline](../../compiler/GHC/Cmm/Pipeline.hs) · [Rust incremental compilation](https://rustc-dev-guide.rust-lang.org/queries/incremental-compilation-in-detail.html)

### Where optimization time survives a small edit

In `GHC.Utils.Misc` we changed the edit-distance helper's `Word32` cutoff from 32 to 31. We compiled both versions with stock GHC 10 at `-O2`. Two repeats of the original gave identical desugared and final Core dumps after removing dump headers and timestamps. We compared complete top-level groups as multisets, retaining printed uniques, types, coercions, binder metadata, recursive groups and right-hand sides.

Of 153 desugared groups, 152 matched after the edit. Of 309 final groups, 308 matched. The only textual difference in each unmatched group was the changed literal. The 1,142 rule-firing records and the printed local rules also matched. Repeated object and interface files agreed within each source version.

The ten simplifier invocations took 3.39–3.69 CPU seconds and allocated 5.405 GB per baseline run. The first invocation alone took 566 ms and allocated 986 MB in the first run. Those are whole-pass costs. To establish the value of a cache, attribute the cost to candidate groups, then record which unfoldings, rewrite rules and other binder information the simplifier actually reads. An unchanged result can depend on a changed optimization input. LLVM ThinLTO includes imported functions and global analysis decisions in its incremental cache key. GHC would need a corresponding account of its own dependencies. [Simplifier iteration](../../compiler/GHC/Core/Opt/Simplify/Iteration.hs) · [ThinLTO design](https://blog.llvm.org/2016/06/thinlto-scalable-and-incremental-lto.html)

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

### RTS cost during compilation

We ran the retained GHC 10.1.20261115.1 compiler with its own threaded dynamic RTS. Four concurrent processes used four physical cores in one Pueue job with a 2 GiB memory cap and swap disabled. Each process compiled a separate copy of its target. GC CPU is summed across the four processes. Peak memory is sampled for their cgroup.

| Four-process workload | Rounds per setting | Median wall, default / 64 MiB nursery | GC CPU, default / 64 MiB | Peak memory, default / 64 MiB |
| --- | ---: | ---: | ---: | ---: |
| Two GHC.Cmm.Sink and two GHC.Data.Bool | 6 | 2.161 / 2.049 s | 1.521 / 1.044 s | 254 / 470 MiB |
| Four GHC.Cmm.Sink | 4 | 2.299 / 2.162 s | 2.148 / 1.586 s | 427 / 636 MiB |

The 64 MiB nursery cut median wall time by 5–6%. Timing ranges overlapped. All objects and interfaces matched. The extra 209–216 MiB matters when several builds share a machine. We kept the existing defaults. The [individual samples](2026-11-27.csv) include smaller nursery settings.

In a separate Callgrind run, sequential evacuation in `evacuate1` used 474,394,570 of 3,386,978,025 instructions (14.0%) in the compiler process building `GHC.Data.Bool`. Both artifacts matched the uninstrumented build. Of 8,694,351 pointer classifications, 4,618,238 took the static path. Of 2,540,069 ordinary closure dispatches across 16 targets, the largest target received 743,727. Removing the suspected extra flag load would save at most 0.12% of the routine's instructions. This profile gives little support for specializing one closure type. The `GHC.Cmm.Sink` profile reached its 100 s limit without usable instruction counts. [Evacuation code](../../rts/sm/Evac.c)
