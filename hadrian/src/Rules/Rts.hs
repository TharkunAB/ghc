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
        buildPath -/- "rust-rts.o" %> buildRustRts stage
        buildPath -/- "libHSghc-internal-*.def" %> buildGhcInternalImportDef
        buildPath -/- "libHSghc-internal-*.dll.a" %> buildGhcInternalImportLib stage

-- | Return the Rust objects to include in the RTS libraries for this stage.
rtsRustObjects :: Stage -> Action [FilePath]
rtsRustObjects stage = do
    rustc <- rtsRustCompiler stage
    if null rustc then pure [] else do
        path <- rtsBuildPath stage
        pure [path -/- "rust-rts.o"]

-- | Compile the Rust RTS code to a PIC object used in all ways.
buildRustRts :: Stage -> FilePath -> Action ()
buildRustRts stage output = do
    rustc <- rtsRustCompiler stage
    when (null rustc) $ fail "The Rust RTS requires --enable-rust-rts."
    _ <- setting RustRtsCompilerVersion
    let source = "rts/rust/lib.rs"
    need [rustc, source, "rts/rust/file_lock.rs"]
    createDirectory (takeDirectory output)
    prog <- exeSpawnPath rustc
    cmdExe prog
        [ "--edition=2024", "--crate-name=ghc_rts", "--crate-type=lib"
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
