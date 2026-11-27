module Rules.Rts (rtsRules, rtsRustObjects) where

import Hadrian.Oracles.ArgsHash (trackArgsHash)
import Hadrian.Target (target)
import Hadrian.Utilities
import Settings.Builders.Common

-- | This rule has priority 3 to override the general rule for generating shared
-- library files (see Rules.Library.libraryRules).
rtsRules :: Rules ()
rtsRules = priority 3 $ do
    root <- buildRootRules
    -- Solve the recursive dependency between the rts and ghc-internal
    -- on Windows by creating an import lib for the ghc-internal dll,
    -- to be linked into the rts dll.
    forM_ [Stage1, Stage2, Stage3 ] $ \ stage -> do
        let buildPath = root -/- buildDir (rtsContext stage)
        buildPath -/- "rust-rts.o" %> \output -> do
            largeAddressSpace <- rtsConfigDefines buildPath "USE_LARGE_ADDRESS_SPACE"
            let rustCfg = if largeAddressSpace then ["--cfg", "ghc_use_large_address_space"] else []
            buildRustRts stage "ghc_rts" "rts/rust/lib.rs"
                [ "rts/rust/archive_member.rs"
                , "rts/rust/arena.rs"
                , "rts/rust/encode.rs"
                , "rts/rust/eventlog_file_writer.rs"
                , "rts/rust/file_lock.rs"
                , "rts/rust/gc_budget.rs"
                , "rts/rust/hash_chain.rs"
                , "rts/rust/hash_insert.rs"
                , "rts/rust/hash_walk.rs"
                , "rts/rust/mblock_extents.rs"
                , "rts/rust/prim_float.rs"
                , "rts/rust/stable_ptr.rs"
                ] rustCfg output
        buildPath -/- "rust-rts-prim-bits.o" %>
            buildRustRts stage "ghc_rts_prim_bits" "rts/rust/prim_bits.rs" [] []
        buildPath -/- "rust-rts-profiler-json.o" %>
            buildRustRts stage "ghc_rts_profiler_json" "rts/rust/profiler_json.rs" [] []
        buildPath -/- "rust-rts-poll-slots.o" %>
            buildRustRts stage "ghc_rts_poll_slots" "rts/rust/poll_slots.rs" [] []
        buildPath -/- "rust-rts-spark-retain.o" %>
            buildRustRts stage "ghc_rts_spark_retain" "rts/rust/spark_retain.rs" [] []
        buildPath -/- "rust-rts-wsdeque.o" %>
            buildRustRts stage "ghc_rts_wsdeque" "rts/rust/wsdeque.rs" [] []
        buildPath -/- "libHSghc-internal-*.def" %> buildGhcInternalImportDef
        buildPath -/- "libHSghc-internal-*.dll.a" %> buildGhcInternalImportLib stage

-- | Return the Rust objects for an RTS library way.
rtsRustObjects :: Stage -> Way -> Action [FilePath]
rtsRustObjects stage way = do
    rustc <- rtsRustCompiler stage
    if null rustc then pure [] else do
        path <- rtsBuildPath stage
        poll <- if Threaded `wayUnit` way
            then pure False
            else rtsConfigDefines path "IOMGR_BUILD_POLL"
        rustDeque <- if Threaded `wayUnit` way
            then do
                let flags = target (rtsContext stage) (Cabal Flags stage) [] []
                trackArgsHash flags
                notElem "thread-sanitizer" <$> interpret flags getArgs
            else pure False
        pure $ [path -/- "rust-rts.o", path -/- "rust-rts-prim-bits.o"]
            ++ [path -/- "rust-rts-profiler-json.o" | Profiling `wayUnit` way]
            ++ [path -/- "rust-rts-poll-slots.o" | poll]
            ++ [path -/- "rust-rts-spark-retain.o" | Threaded `wayUnit` way]
            ++ [path -/- "rust-rts-wsdeque.o" | rustDeque]

-- | Check an RTS feature macro in the generated configuration.
rtsConfigDefines :: FilePath -> String -> Action Bool
rtsConfigDefines path name = do
    autoconf <- readFile' (path -/- "include/ghcautoconf.h")
    pure $ any ((== ["#define", name, "1"]) . words) (lines autoconf)

-- | Compile a Rust RTS crate to a PIC object.
buildRustRts :: Stage -> String -> FilePath -> [FilePath] -> [String] -> FilePath -> Action ()
buildRustRts stage crate source modules extraArgs output = do
    rustc <- rtsRustCompiler stage
    when (null rustc) $ fail "The Rust RTS requires --enable-rust-rts."
    _ <- setting RustRtsCompilerVersion
    need [sourcePath -/- "Rules/Rts.hs"]
    need (rustc : source : modules)
    createDirectory (takeDirectory output)
    prog <- exeSpawnPath rustc
    cmdExe prog $
        [ "--edition=2024", "--crate-name=" ++ crate, "--crate-type=lib"
        , "--target=x86_64-unknown-linux-gnu", "--emit=obj"
        , "-Cpanic=abort", "-Crelocation-model=pic", "-Ccodegen-units=1"
        , "-Copt-level=2", "-o", output, source
        ] ++ extraArgs

buildGhcInternalImportDef :: FilePath -> Action ()
buildGhcInternalImportDef target = do
    templateIn <- readFile' "rts/win32/libHSghc-internal.def.in"
    let dllName = takeFileName target -<.> "dll"
        templateOut = replace "@GhcInternalDll@" dllName templateIn
    writeFileAtomic target templateOut

buildGhcInternalImportLib :: Stage -> FilePath -> Action ()
buildGhcInternalImportLib stg target = do
    let input = dropExtensions (dropExtension target) <.> "def" -- the .def file
        output = target -- the .dll.a import lib
    need [input]
    runBuilder (Dlltool stg) ["-d", input, "-l", output] [input] [output]
