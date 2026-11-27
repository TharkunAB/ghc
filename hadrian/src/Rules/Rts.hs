module Rules.Rts (rtsRules, rtsRustObjects) where

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
        buildPath -/- "rust-rts.o" %>
            buildRustRts stage "ghc_rts" "rts/rust/lib.rs"
                [ "rts/rust/archive_member.rs"
                , "rts/rust/eventlog_file_writer.rs"
                , "rts/rust/file_lock.rs"
                , "rts/rust/gc_budget.rs"
                ]
        buildPath -/- "rust-rts-profiler-json.o" %>
            buildRustRts stage "ghc_rts_profiler_json" "rts/rust/profiler_json.rs" []
        buildPath -/- "libHSghc-internal-*.def" %> buildGhcInternalImportDef
        buildPath -/- "libHSghc-internal-*.dll.a" %> buildGhcInternalImportLib stage

-- | Return the Rust objects for an RTS library way.
rtsRustObjects :: Stage -> Way -> Action [FilePath]
rtsRustObjects stage way = do
    rustc <- rtsRustCompiler stage
    if null rustc then pure [] else do
        path <- rtsBuildPath stage
        pure $ (path -/- "rust-rts.o") :
            [path -/- "rust-rts-profiler-json.o" | Profiling `wayUnit` way]

-- | Compile a Rust RTS crate to a PIC object.
buildRustRts :: Stage -> String -> FilePath -> [FilePath] -> FilePath -> Action ()
buildRustRts stage crate source modules output = do
    rustc <- rtsRustCompiler stage
    when (null rustc) $ fail "The Rust RTS requires --enable-rust-rts."
    _ <- setting RustRtsCompilerVersion
    need [sourcePath -/- "Rules/Rts.hs"]
    need (rustc : source : modules)
    createDirectory (takeDirectory output)
    prog <- exeSpawnPath rustc
    cmdExe prog
        [ "--edition=2024", "--crate-name=" ++ crate, "--crate-type=lib"
        , "--target=x86_64-unknown-linux-gnu", "--emit=obj"
        , "-Cpanic=abort", "-Crelocation-model=pic", "-Ccodegen-units=1"
        , "-Copt-level=2", "-o", output, source
        ]

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
