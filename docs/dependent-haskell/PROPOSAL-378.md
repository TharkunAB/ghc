# GHC Proposal #378 Deep Audit & Wave 4–13 Closure Record

**Target branch:** `dependent-haskell` (`https://github.com/TharkunAB/ghc`)  
**Current status (Wave 13):** All Priority 1 (`P1.1`–`P1.13`), Priority 2 (`R01`–`R50` within Haskell's non-strict design boundaries), and Priority 3 (`P3.1`–`P3.8`) deliverables audited below are **implemented and verified** through Wave 12 (`f15bf516fb`, 642 Showcase observations, 88 registered dependent-Haskell tests, `234/234` compatibility, `138/138` focused, `100/100` PLFH) and Wave 13 (standalone [`dependent-base`](https://github.com/TharkunAB/dependent-base) migration and PiTA `#9` point-free `foreach` typed-hole fits). Three explicit boundaries remain documented by design: `R27` retains `CUSKs` by default in `Haskell98`/`Haskell2010` while `-XDependentTypes` implies `StandaloneKindSignatures` (`NoCUSKs`); `R34` implements surface types-in-terms, T2T operator sections, and the `data` herald while keeping `HsExpr` and `HsType` separate internal AST types (`#25121`); and type-level promotion certifies structural/pure definitions without assuming totality or promoting arbitrary `IO`/effectful code (`#378` §4.10, §4.13).  
**Original audited baseline:** Normal compiler `844f309834` + Wave 1 (`69a36b132f`) + Wave 2 Lane 1 (`03b7bb9a00`) · PLFH Batch 37 (`8756aed`, 21/25 chapters `Checked`, 38/38 fixed regressions, 14 open reproducers)  
**Primary specification:** [GHC Proposal #378 — *Design for Dependent Types*](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst) (accepted 2021-05-26, master revision, all 1,376 lines audited)  
**Cross-checked specifications:** GHC Proposals `#81`, `#99`, `#102`, `#106`, `#126`/`#291`, `#155`, `#242`, `#270`, `#274`, `#281` (amended by `#626`), `#282`/`#583`, `#287`, `#364`, `#370`, `#387`, `#402`, `#425`, `#448`, `#473`, `#475`, `#509`, `#523`, `#547`, `#581`, `#765`; [Serokell Dependent Haskell Roadmap](https://ghc.serokell.io/dh) (`R01`–`R50`); Agda 2.8.0 & PLFA `dev`.

---

## 1. Design Authority Hierarchy (Codify into `HANDOFF.md` & `ROADMAP.md`)

All language design, surface syntax, flag/extension structure, name resolution, type inference, and library decisions on this fork follow a strict four-tier authority hierarchy:

1. **Tier 1 — GHC Proposal #378 (*Design for Dependent Types*):**  
   Follow accepted **GHC Proposal #378** whenever it specifies or constrains a feature:
   - Core principles: **Type Inference** (§4.1), **Predictable Erasure Principle / PEP** (§4.2), **Lexical Scoping Principle / LSP** (§4.3.1–4.3.2), **Syntactic Unification Principle / SUP** (§4.3.3), **Opt-In Principle / OIP** (§4.12), and **Non-Totality by Default** (§4.10, §4.13).
   - The 6-quantifier grid (§4.4): `forall a.`, `forall a ->`, `foreach a.`, `foreach a ->`, `c =>`, `t1 -> t2`.
   - Dependent pattern matching in bidirectional checking mode (§4.5), term variables as skolems in types (§4.6), dependent application in term syntax over the Static Subset (§4.7), explicit signatures for dependent definitions (§4.8), phase distinction (§4.9), and a native typed Core intermediate language (§4.10).
2. **Tier 2 — Other GHC Proposals (Accepted first, then Active DH Drafts):**  
   For features delegated by `#378` or outside its text, follow the dedicated GHC proposals:
   - **Accepted:** `#281` (Visible `forall`, `type` herald, types-in-terms, T2T mapping, term-variable capture), `#242` (Unsaturated type families & `Matchability`), `#370` (Modifiers `%m`), `#155` & `#448` (`TypeAbstractions` `@a` in lambdas/equations & patterns), `#402` (Stable/nested GADT syntax), `#425` (Invisible `@k` binders in type declarations), `#475` (`NoListTuplePuns`), `#581` (Namespace-specified imports), `#765` (`-Wpun-uses` / `-Wpun-bindings`), `#282`/`#583` (`OverloadedRecordDot` / `HasField`), `#364`/`#387` (`Natural` / `Char` promotion), `#274` (Quick Look impredicativity).
   - **Active / Tabled DH Proposals:** `#102` (`-XDependentTypes`, `foreach` / Unicode `∏` `U+220F`, quantifier matchability), `#473` (`-XExistentialTypes`, `exists tv_bndrs .` / `|`, `GHC.Exists` `Pack`, `@Pack`, `/\`, `Witness`), `#509` (Type functions / closed compile-time evaluation), `#523` (`-XExtendedLet`: local `type` synonyms & `let`/`where` in types), `#177`/`#259` (Constrained type families).
3. **Tier 3 — Serokell Dependent Haskell Roadmap (`https://ghc.serokell.io/dh`, `R01`–`R50`):**  
   Use the Serokell roadmap for task decomposition, dependency ordering, and concrete designs not yet fixed by a GHC proposal — notably `some (a :: A) | F a` for retained Σ-types (`R02`/`R04`), open term evaluation (`R16`), inline type-level `\x ->` and `case ... of` (`R17`/`R19`), top-level mixed term/type dependency analysis (`R21`), unboxed & data-family promotion (`R10`/`R14`), and homogeneous equality (`R11`).
4. **Tier 4 — Agda (and PLFA / PLFH Textbook Ergonomics):**  
   Where Tiers 1–3 leave ergonomics open (e.g. grouped binder telescopes `foreach (m n p :: Nat) ->`, dependent record projections/updates, coverage refinement across GADT witnesses, `with`-abstraction ergonomics, `dependent-base` / `HaskLib` combinators, optional advisory totality checking), draw inspiration from **Agda** and PLFA — subject always to Haskell's non-strict evaluation (no silent forcing or function eta-expansion), Predictable Erasure, and Lexical Scoping.

> **Migration Rule for Existing Fork Code:** Wherever our fork previously invented custom syntax, flags, or ad-hoc restrictions that diverge from Tier 1 or Tier 2, **make the `#378` / GHC-proposal form canonical immediately** while keeping existing fork spellings as compatibility aliases (with optional deprecation warnings) until `Showcase.hs` (627 observations), `HaskLib`, and `PLFH` (34 modules) are migrated cleanly.

---

## 2. Proposal #378 Section-by-Section Conformance Matrix

| `#378` Section | Specification Requirement | Current Fork Status | Priority / Action |
| :--- | :--- | :--- | :--- |
| **§4.1 Type inference** | Every valid Haskell program remains valid; annotations required only for dependent/higher-rank/GADT features via predictable rules. | **Partial:** Ordinary code works, but retained calls create untouchable meta-variables (`R03_UntouchableRetainedCallPoly.hs`) and unsigned eta-aliases of some dependent functions fail (`RetainedInference`). | **P1.10 / P2** |
| **§4.2 Erasure (PEP)** | Programmer knows from the type (`forall`/`exists` vs `foreach`/`some`) what is erased vs retained; explicit in datatypes. | **Aligned** in Core (`ForAllTy`/`ExistsTy` erased; `DepFunTy`/`Sigma` retained), except `foreach (a :: Type)` leaks `TypeRep` into user types. | **P1.6** |
| **§4.3.2 Lexical scoping (LSP)** | Primary namespace first, fallback to secondary namespace; implicit quantification **only** when unbound in *both* namespaces. | **Divergent in corners:** `RequiredTypeArguments` handles top-level `filterInScopeM`, but `filterGadtConTyVars`, `rnLocalHsSigWcType`, and `filterSourceTableNames` use inconsistent ad-hoc rules. | **P1.8** |
| **§4.3.3 Syntactic Unification (SUP)** | Long-term: in the absence of punning, type-syntax and term-syntax coincide (`R34`). | **Partial:** `HsExpr` and `HsType` remain separate; `foreach`/`exists`/`some` cannot even be parsed in term-syntax (`infixexp2`), and large eliminations (`Val -> Type`) cannot return unadorned types. | **P1.4, P1.7, P2** |
| **§4.3.4 Switching syntaxes** | `type` herald switches to type-syntax; `data` herald switches to term-syntax. | **Partial:** `type` herald works in expressions/patterns (`#281`) and imports (`#581`); `data` herald works in imports/exports (`#581`) but not yet as an expression/type escape herald (`R34`/`R35`). | **P2 (`R34`)** |
| **§4.4 Quantifiers** | 6 quantifiers: `forall a.`, `forall a ->`, `foreach a.`, `foreach a ->`, `Eq a =>`, `t1 -> t2`. Unannotated `foreach a ->` / `foreach a.` allowed; `_` allowed (`foreach (_ :: t1) -> t2`). | **Divergent:** Fork rejects unannotated `foreach a ->` (`RetainedBinderNeedsType`), rejects wildcard `foreach (_ :: t) ->` (`RetainedBinderNeedsName`), forbids `{a :: t}` specificity in `foreach_bndrs`, and lacks Unicode `∏` (`#102`). | **P1.3** |
| **§4.5 Dependent pattern-match** | Pattern-matching a `foreach`-bound value in *checking* mode refines types on the RHS. | **Partial / Over-restricted:** Works for unguarded top-level constructor equations and flat `case`, but fork **rejects** guards, nested constructor patterns (`S (S n)`), catch-all `_` branches in dependent `case`, and TH quotes. | **P1.9** |
| **§4.6 Term variables in types** | Each term variable used in a type is its own skolem (equal only to itself); optional later extension for Static-Subset `let` RHSs. | **Inconsistent hybrid:** Core `TermVarTy` is a skolem, while local scalar `let` (and 1-clause unguarded `where`) unfolds arithmetic RHSs, yet local composite functions (`to x = h (f x)`) and multi-clause `where` stay opaque (`R04_ComposedIsoFields.hs`). | **P1.11** |
| **§4.7 Dependent application & Static Subset** | Arguments to `foreach ->` and `forall ->` are parsed/renamed in **term syntax**; `forall`, `->`, `foreach` are valid term-syntax (rejected by TC in actual terms); Static Subset widens incrementally. | **Divergent:** (1) `foreach`/`exists`/`some` are missing from `infixexp2` & `T2T`; (2) invisible `foreach .` uses custom `@{expr}` instead of `@arg`; (3) inline GADT constructors with indices (`CHole`) or existentials (`App l' m`) fail Static-Subset admission (`GHC-83605`). | **P1.2, P1.4, P1.10** |
| **§4.8 Dependent definition** | Never infer `foreach .`, `foreach ->`, `forall ->`. Invisible `foreach (n :: Nat).` supports **both** `ScopedTypeVariables` (`case n of ...`) **and** `@`-binder patterns (`vReplicateImplicit @Zero _ = Nil`, `quickLength @_ @n _ = n`). | **Divergent:** Invisible `foreach .` only supports implicit `ScopedTypeVariables` capture; writing `@n` or `@Zero` / `@(Succ n)` at definition sites is **not implemented** (`addInvisibleRetainedPatterns` blindly inserts hidden `VarPat`s after erased `@` patterns!). | **P1.2** |
| **§4.9 Phase distinction** | Erased args cannot be used at runtime; retained `foreach (a :: Type) -> a` can pass `a` to `forall (a :: Type) ->` (`the a undefined`) and eventually match `case a of Bool -> ...`; `MkT :: forall a -> foreach b -> X a b -> T` allowed. | **Divergent:** (1) `foreach (a :: Type) ->` desugars `a` to a `TypeRep a` term + hidden `forall {a}`, breaking `ex5 a = the a undefined` and requiring `(type Char)` at call sites; (2) GADT constructors forbid mixing existential `forall` with `foreach` (`RetainedPackageDeclaration`) and forbid `foreach .` (`RetainedInvisibleField`). | **P1.6, P1.9** |
| **§4.10 Expressiveness & Typed Core** | No mandatory termination checker; native typed dependent Core (`GraD` / `System DR` / `DC`). | **Aligned in principle, partial in Core:** No termination requirement; native `DepFunTy`/`TermVarTy`/`TermAppTy`/`ExistsTy` exist, but Desugar Core Lint still fails on nested GADT coercions (`R05_NestedFinForEachCoreLint.hs`) and general `-O2` unfolding serialization for native Core remains gated. | **P2 (`R05`)** |
| **§4.12 Opt-In Principle (OIP)** | Opt-in via `-XDependentTypes` (single umbrella extension, `#102` §Spec) or importing a DH module; non-DH users unaffected in types and diagnostics. | **Divergent:** Extension is named `-XExperimentalDependentTypes` (no `-XDependentTypes`), implies *only* `RequiredTypeArguments` (forcing 8–15 pragmas per module), and requires hidden `-fwrite-source-equations` + `-fno-enable-rewrite-rules` flags! | **P1.1, P1.5** |

---

## 3. Priority 1 — Already-Implemented Fork Features That DIVERGE from Proposal #378 (HIGHEST PRIORITY)

These 13 items are places where our fork **already built machinery**, but used custom syntax, non-standard compiler flags, or over-restrictive gates that contradict **Proposal #378** (Tier 1) or its directly delegated proposals (`#281`, `#242`, `#370`, `#473`, `#509`, `#102`). Fixing Priority 1 aligns the existing compiler with the official specification before expanding surface area.

### P1.1 — Extension Name & Pragma Soup (`-XExperimentalDependentTypes` vs `#378` `-XDependentTypes` Bundle)
- **Authoritative Spec:**
  - `#378` §4.12 (line 1002): *"By 'opt into', we mean that users would have to enable `-XDependentTypes`..."*
  - `#102` §Proposed Change Specification (lines 186–191): *"Introduce a new GHC extension, `-XDependentTypes`. This extension would be a catchall for the dependent features being proposed here and elsewhere... By introducing one new extension, we avoid the need for many extensions for different slices of the Dependent Haskell feature set."*
  - `#473` §2.1 (line 145): `-XExistentialTypes` as the standalone flag for `exists`.
  - `#523` §1.2 (line 54): `-XExtendedLet` as the standalone flag for `let` in types / local `type` synonyms.
- **Current Fork Divergence:**
  - `libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs` and `compiler/GHC/Driver/Flags.hs:208,379` only define `ExperimentalDependentTypes`, and `impliedXFlags` (`Flags.hs:379`) **only** implies `RequiredTypeArguments`.
  - Meanwhile `docs/users_guide/exts/experimental_dependent_types.rst:686` contradicts line 9 by claiming *"The extension implies no other extensions."*
  - Result: PLFH modules require an average of **8.6** and up to **15 `LANGUAGE` pragmas** per module (`Showcase.hs:1-26` has 26 pragmas!). Standalone `#473` (`ExistentialTypes`) and `#523` (`ExtendedLet`) flags do not exist (`GHC/Parser/Lexer.x:1084` gates `exists` on `ExperimentalDependentTypesBit`; `GHC/Rename/HsType.hs:1257` gates type `let` on `ExperimentalDependentTypes`).
- **Required Alignment:**
  1. Add `LangExt.DependentTypes` (and standalone `LangExt.ExistentialTypes`, `LangExt.ExtendedLet`) in `GHC.LanguageExtensions.Type`, `GHC.Driver.Flags`, and Cabal/`ghc-boot-th`.
  2. Have `DependentTypes` imply `ExperimentalDependentTypes` (and vice versa, or keep `ExperimentalDependentTypes` as the legacy alias) **plus** the standard DH core bundle in `impliedXFlags`:
     `RequiredTypeArguments`, `ExplicitNamespaces`, `DataKinds`, `PolyKinds`, `GADTs` (which implies `GADTSyntax`, `MonoLocalBinds`), `TypeOperators`, `RankNTypes` (implies `ExplicitForAll`), `ScopedTypeVariables`, `TypeApplications`, `TypeAbstractions`, `StandaloneKindSignatures`, `EmptyCase`, `Modifiers`, `UnsaturatedTypeFamilies` (or keep `TypeFamilies` opt-out via `NoTypeFamilies` if desired — note `GADTs` already implies `MonoLocalBinds`!), `ExistentialTypes`, `ExtendedLet`. (Every implied extension remains overridable via `No...` placed after `DependentTypes`.)
  3. Fix the contradictory paragraph in `docs/users_guide/exts/experimental_dependent_types.rst:686-689`.

---

### P1.2 — Invisible `foreach .` Override Syntax: Custom `@{expr}` vs Canonical `@` (`TypeApplications` / `TypeAbstractions`)
- **Authoritative Spec:**
  - `#378` §4.8 (lines 826–836):
    ```haskell
    vReplicateImplicit :: foreach (n :: Nat). a -> Vec n a
    vReplicateImplicit @Zero     _ = Nil
    vReplicateImplicit @(Succ _) x = x :> vReplicateImplicit x
    ```
  - `#378` §4.13 (lines 1204–1206):
    ```haskell
    quickLength :: forall (a :: Type). foreach (n :: Nat). Vec a n -> Nat
    quickLength @_ @n _ = n
    ```
  - Serokell `R28` (*Invisible type arguments*) & `R32` (*Visible type applications*): *"Currently this syntax [`f @t` / `@a`] works with polymorphic functions of type `forall a. t`, and we intend to reuse the same mechanism to visibly supply [and bind] the arguments of dependent functions of type `foreach a. t`."*
- **Current Fork Divergence:**
  - **Call sites (`compiler/GHC/Parser.y:3194-3199`, `GHC/Rename/Expr.hs:394-400`, `GHC/Tc/Gen/Head.hs:189,284,327`, `GHC/Tc/Gen/App.hs:4067-4081`, `GHC/Tc/Errors/Ppr.hs:3768-3772`):**  
    The fork invented a bespoke `f @{n}` syntax (`RETAINED_AT '{' exp '}'` -> `HsAppRetained` -> `ERetainedArg`), and `RetainedInvisibleArgument` (`Ppr.hs:3770`) instructs users: *"Supply an argument with `@{...}`"*. Writing standard `#378` `f @n` or `f @Zero` parses as `HsAppType` and fails because `tcInstFun` only consumes `ForAllTy` (not `DepFunTy DepFunInvisible`) for `ETypeArg`!
  - **Definition sites (`compiler/GHC/Rename/Bind.hs:716-742`, `compiler/GHC/Tc/Gen/Match.hs:195-210`):**  
    `addInvisibleRetainedPatterns` (`Match.hs:204-210`) unconditionally synthesizes fresh `VarPat`s from the signature's `foreach .` binder names, supporting *only* `ScopedTypeVariables`-style implicit capture. Writing `#378`'s `quickLength @_ @n _ = n` or dependent pattern-matching `vReplicateImplicit @Zero _ = Nil` fails because `InvisPat` (`@pat`) is not matched against `DepFunTy DepFunInvisible`!
- **Required Alignment:**
  1. **Call sites (`GHC/Tc/Gen/App.hs`, `GHC/Rename/HsType.hs`):** When `tcInstFun` encounters `ETypeArg` (`f @arg`) and the function head is `DepFunTy DepFunInvisible binder body`, elaborate `arg` (which in Static Subset already covers constructors, literals, local runtime indices via `Note [Runtime indices in visible type arguments]`, and promoted calls!) into the runtime/type operand for `binder`, substituting into `body` and passing the runtime value in Core. Keep `f @{expr}` as a backward-compatible alias (and fallback for non-`atype` expressions until `HsExpr`/`HsType` unify). Update `Ppr.hs:3770` to recommend `@arg`.
  2. **Definition sites (`GHC/Rename/Bind.hs`, `GHC/Tc/Gen/Match.hs`, `GHC/Tc/Gen/Pat.hs`):** Allow `InvisPat` (`@n`, `@_`, `@Zero`, `@(Succ k)`) in function equations and `\ @n -> ...` lambdas to bind and/or constructor-refine `DepFunTy DepFunInvisible` binders (falling back to the signature's scoped binder name when no explicit `@pat` is written, preserving `#378` lines 826–830!).

---

### P1.3 — `foreach` Telescope Restrictions vs `#378` §4.4, §4.11, §4.13 & `#102`
- **Authoritative Spec:**
  - **Unannotated binders (`#378` §4.4 lines 518–519 `foreach a. ty` / `foreach a -> ty`, §4.11 line 966 `foreach a -> blah`, §4.13 line 1221):** *"The kind annotations `(:: Type` and `:: Nat)` are unnecessary and could be inferred."*
  - **Wildcard binder (`#378` §4.11 lines 978, 984):** `foreach (_ :: t1) -> t2` (restricts call argument to Static Subset without binding a variable in `t2`).
  - **Inferred specificity braces (`#102` line 195 + `#99`):** `foreach` is *"syntactically identical in behavior to `forall`"*, so `foreach {n :: Nat}. ty` / `foreach {n}. ty` marks an `InferredSpec` invisible retained binder (skipped by explicit `@` overrides, inferred from arguments/result).
  - **Unicode `∏` (`#378` lines 528, 1212; `#102` lines 196–201):** With `-XUnicodeSyntax` and `-XDependentTypes`, `∏` (`U+220F`, N-ARY PRODUCT) is the Unicode token for `foreach`.
- **Current Fork Divergence:**
  - `compiler/GHC/Rename/HsType.hs:520-522` (`rnRetainedTelescope`):
    - Line 520: `HsTvb _ _ HsBndrWildCard{} _` -> **fails** with `RetainedBinderNeedsName` (*"A retained argument requires a variable name"*), even though `GHC/Parser.y:2613` (`tyvar_wc`) explicitly parses `_`!
    - Line 522: `HsBndrNoKind` (`foreach n -> Vec a n`) -> **fails** with `RetainedBinderNeedsType` (*"A retained argument requires an explicit domain type"*).
  - `compiler/GHC/Parser.y:2577-2580` (`foreach_bndrs`): Uses `tv_bndr_no_braces` instead of `tv_bndr` for `foreach .`, **rejecting** `{n}` / `{n :: Nat}` at parse time!
  - `compiler/GHC/Parser/Lexer.x:815,1163-1176`: `ITforeach` has no `IsUnicodeSyntax` field and `reservedSymsFM` has no `("∏", ITforeach UnicodeSyntax, UnicodeSyntax, xbit ExperimentalDependentTypesBit)` entry.
- **Required Alignment:**
  1. Allow wildcard `_` in `rnRetainedTelescope` and `tcCheckRetainedType`: allocate an internal fresh `Id` (unreferenced in `body`, keeping Static-Subset checking on the argument at call sites per `#378` line 984!).
  2. Allow `HsBndrNoKind` (`foreach n -> Vec a n` / `foreach n. Vec a n`): allocate a fresh unification kind/type meta-variable `alpha :: Type` before checking the body `Vec a n`, which unifies `alpha ~ Nat` from `n`'s usage in `Vec a n`, then validate `zonkTcType alpha` with `tcRetainedBinderDomain`!
  3. Permit `{n}` / `{n :: a}` (`InferredSpec`) on invisible `foreach .` (rejecting `{...}` on visible `foreach ->` with the exact same `fromSpecTyVarBndrs` check `forall ->` uses at `GHC/Parser.y:2287`!).
  4. Add Unicode `∏` (`U+220F`) to `GHC/Parser/Lexer.x` under `UnicodeSyntax` + `DependentTypes`.

---

### P1.4 — Missing `foreach`, `exists`, `some` (and Type Lambdas/Lets/Cases) in Term Syntax & `T2T` (`#378` §4.7 & `#281` Part II)
- **Authoritative Spec:**
  - `#378` §4.7 (lines 791–804): *"Dependent application also requires us to extend term-syntax to include all types... Although these type-like forms (function arrow, forall, foreach) are now valid term-syntax, accepted anywhere in term-syntax by the parser and renamer, they are rejected by the typechecker in actual terms."*
  - `#281` Part II (lines 642–720, 934, 986): Types-in-terms (`infixexp2` / `infixpat2`) are mapped directly by `T2T` when checking an argument to `forall a ->` (and `foreach (a :: Type) ->`).
- **Current Fork Divergence:**
  - `compiler/GHC/Parser.y:3042-3087`: `infixexp2` includes `->`, `=>`, and `forall_telescope`, **but omits `foreach`, `exists`, and `some`**! Writing `idv (foreach (n :: Nat) -> Vec Int n)` is a syntax error unless wrapped in `type (...)`.
  - `compiler/GHC/Tc/Gen/App.hs` (`expr_to_type` / T2T): Does not map `foreach`, `exists`, `some`, or type-level `\x -> ...` / `let ... in ...` / `case ... of ...` when `T2T` converts a required type argument!
- **Required Alignment:**
  Extend `infixexp2` (and `DisambECP` in `GHC/Parser/PostProcess.hs`), `rnExpr` (rejecting in actual term positions at TC time just like `HsForAll` in terms), and `expr_to_type` (`T2T`) to support `foreach`, `exists`, and `some` seamlessly without requiring `type (...)`.

---

### P1.5 — Requiring `-fwrite-source-equations`, `-fno-enable-rewrite-rules`, and the Same-Module vs Cross-Module Promotion Split (`#378`, `#509`, `R13`/`R16`/`R21`)
- **Authoritative Spec:**
  - `#378` §4.3.2, §4.6, §4.11 & Serokell `R13`/`R16`/`R21`: Ordinary term functions are promoted for use in types under `-XDependentTypes` without special hidden `-f*` flags, and `R21` (*Dependency analysis*) specifically mandates that types and terms in the **same module** can depend on each other in topological order without splitting modules!
- **Current Fork Divergence:**
  1. **Hidden `-fwrite-source-equations` flag (`compiler/GHC/Driver/Flags.hs:928`, `GHC/Driver/Session.hs:2629`, `GHC/Driver/Main/Passes.hs:931-946`):**  
     Publishing closure-capturing and higher-order source equations (`publishCheckedSourceClosures`) is gated on `gopt Opt_WriteSourceEquations dflags && xopt LangExt.ExperimentalDependentTypes dflags && null (tcg_rules tc_result)`. Users have to sprinkle `{-# OPTIONS_GHC -fwrite-source-equations #-}` in provider modules (`BigStep.md`, `Compositional.md`, `More.md`, `DeBruijn.md`, `ReproOpsPlug.hs`, `ReproOpsDoubleSubst.hs`).
  2. **Same-module `GHC-68947` failure (`R13_SameModulePlugOpenEq.hs`, `R21_SameModuleSubstSingleOpenEq.hs`):**  
     Because `publishCheckedSourceClosures` runs *after* `tcRnModule` (in `GHC/Driver/Main/Passes.hs:931`, post-desugar/simplify), functions like two-index GADT `plug` and closure-passing `substSingle` work **cross-module** via `.hi` files, yet **fail with `GHC-68947` inside their own defining module**! This forced 7 artificial `clients/` split modules across PLFH.
  3. **`-O2` `base` list-fusion rewrite-rule failures (`R03_FunctionColumnSplit.hs`, `R13_ShuntAccumOpenEq.hs`, `R13_RevOpenEq.hs`):**  
     At default `-O2`, standard `base` `foldr/build` and `(++)` rewrite rules trigger `RetainedFinalCoreRulesEnabled` (`GHC-83605`) or withhold `[x]` equations (`GHC-70565`), forcing users to add `{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}` to ordinary list proof modules!
- **Required Alignment:**
  1. Make `DependentTypes` / `ExperimentalDependentTypes` enable source-equation publication **by default** (keep `-fwrite-source-equations` / `-fno-write-source-equations` only as an optional toggle, defaulting to `On` when `DependentTypes` is active, and drop the blanket `null (tcg_rules tc_result)` gate in `Passes.hs:934` in favor of per-function rule-interference checking!).
  2. Run early source-table / closure synthesis during R21 declaration scheduling in `GHC/Tc/Module.hs` + `GHC/Tc/Gen/SourceTable.hs` (matching how Wave 1 Lane 2 handled `lookupEnv`), with post-tidy verification in `Passes.hs`, eliminating the same-module vs cross-module split (`R13_SameModulePlugOpenEq.hs` & `R21_SameModuleSubstSingleOpenEq.hs`).
  3. Certify `base`'s built-in `GHC.Internal.Base` list `build`/`augment`/`map`/`(++)` semantics-preserving rules (or evaluate open equations against pre-rule certified source definitions when only standard `base` fusion rules fire on pure structural spine consumers), eliminating `-fno-enable-rewrite-rules` on standard list proofs at `-O2`!

---

### P1.6 — `foreach (a :: Type) ->` Desugaring Breaks Phase Distinction & Unadorned Call Syntax (`#378` §4.7 & §4.9)
- **Authoritative Spec:**
  - `#378` §4.9 (lines 868–872):
    ```haskell
    the :: forall (a :: Type) -> a -> a
    the b x = (x :: b)

    ex5 :: foreach (a :: Type) -> a
    ex5 a = the a undefined   -- OK: even though a is retained, can still pass to a function expecting an erased argument
    ```
  - `#378` §4.7 (lines 722–742, 793–796): Callers pass unadorned types in term syntax: `ex5 Int`, `lookupColumn Char chars pos` (not `lookupColumn (type Char) ...`!).
- **Current Fork Divergence:**
  - `compiler/GHC/Tc/Gen/HsType.hs:695-707` (`Note [Explicit retained Type witnesses]`): Translates `foreach (a :: Type) -> body` into `forall {a_tv :: Type}. TypeRep a_tv -> body`!
    1. Inside the function body (`RuntimeTypeColumn.hs:17`), the bound variable `a` is **only** a term variable of type `TypeRep a_tv` — `a` is **not** bound as a type variable `a_tv`! So `#378`'s `ex5 a = the a undefined` and `(x :: a)` **fail**!
    2. At call sites (`Showcase.hs:1095`, `GHC/Tc/Gen/Expr.hs:1318`, `Ppr.hs:3845`), checking against `TypeRep t` accepts `(type Char)` (`HsEmbTy`), **but rejects unadorned `Char`** (`Column.lookupColumn Char chars pos`)!
    3. In `.hi` interfaces (`HsType.hs:926`), *"The interface does not record the original `foreach` spelling"*, leaking `TypeRep` into `:type` output and Haddock!
- **Required Alignment:**
  1. At **definition sites** of `foreach (a :: Type) ->` (and `foreach (a :: Type).`), bind `a` so it is usable **both** in type positions / `T2T` (resolving to the skolem `a_tv :: Type`, making `ex5 a = the a undefined` and `x :: a` work per `#378` line 869!) **and** at runtime via `Typeable a_tv` / `TypeRep a_tv` (including `withTypeable` evidence in scope inside the body!).
  2. At **call sites**, allow unadorned type expressions in the Static Subset (`lookupColumn Char chars pos`, `lookupColumn (Maybe Int) ...`) via `T2T` + `Typeable` synthesis, while continuing to accept `(type Char)` and explicit `TypeRep` values for backward compatibility!

---

### P1.7 — Large Eliminations (`Val -> Type`): Forcing `type family` Instead of Functions Returning `Type` (`#378` §4.3.3, §4.11, §4.13)
- **Authoritative Spec:**
  - `#378` §4.3.3 (SUP) & §4.13 (lines 1159–1161): Dependent Haskell eliminates the duplication of writing `type family` alongside term functions. A function `valPred :: Val -> Type` defined by pattern matching on `Val` is a valid type-computing function!
- **Current Fork Divergence:**
  - Demonstrated by `docs/dependent-haskell/plfh/reproducers/R13_LargeElimTermToType.hs` (and `Showcase.hs:298` `type family Select`):
    ```haskell
    valPred :: Val -> Type
    valPred Bot       = type ()                      -- or Unit / ()
    valPred (Arr a b) = type (valPred a -> valPred b) -- or valPred a -> valPred b
    ```
    Fails (`GHC-83605` / `TcRnUnexpectedTypeSyntaxInTerms` / `GHC-70566`) because term-level functions returning `Type` cannot construct `Type` values on their RHS nor promote into closed type families! This is why `PLFH.Part3.Adequacy.LogVLam` remains the **last manual `type family`** in all 25 chapters of PLFH.
- **Required Alignment:**
  Support **large eliminations** (`f :: D -> Type`, and multi-argument `f :: D1 -> D2 -> Type`): when a function's result type is `Type` (via `Data.Kind.Type`), check each equation RHS using `T2T` / `HsEmbTy` (`type (...)` or unadorned types-in-terms `Unit`, `a -> b`, `forall ...`, etc.), synthesize the corresponding promoted `SourceTable` closed type family `T:f :: D -> Type` automatically for type-level calls `valPred v`, and compile the runtime representation cleanly (either `TypeRep`-erased/error or `Type` token, respecting PEP since `Type` values in types are erased!).

---

### P1.8 — Inconsistent Lexical Scoping & Implicit Quantification Rules (`#378` §4.3.2 & `#281` §6–7)
- **Authoritative Spec:**
  - `#378` §4.3.2 (lines 409–428) & `#281` §6–7 (lines 788–846):
    1. Type-syntax looks up in the type namespace first, then falls back to the term namespace.
    2. Term-syntax looks up in the term namespace first, then falls back to the type namespace.
    3. Implicit quantification in signatures quantifies **only** variables in scope in **neither** namespace.
- **Current Fork Divergence:**
  - `compiler/GHC/Rename/HsType.hs:2913-2984`: Instead of one uniform `#378` / `#281` rule, the fork layers three overlapping mechanisms:
    - `filterInScopeM` (`line 2897`) calls `filterSourceTableNames` (`line 2975`), which queries `lookupSourceTables` during renaming!
    - `filterGadtConTyVars` (`line 2913`) uses bespoke AST-scraping (`field_indices`, `application_heads`) with special exceptions for record field names.
    - `rnLocalHsSigWcType` (`lines 156-161`, `279-311`) only aliases `LocalRdrEnv` names and hard-fails if a name is in `bound_names` (`RetainedLocalSignatureIndex`) or if a wildcard exists anywhere in the signature (`RetainedPartialSignature`).
    - When top-level functions are mutually ordered without an earlier dependent signature (`PLFH-NOTES.md:1038`, `shunt` before `rev`), this split can trigger internal error `GHC-76329` (*not in scope during type checking, but it passed the renamer*).
- **Required Alignment:**
  Unify name resolution and implicit quantification on `#281`'s `CaptureTermVars` rule across top-level signatures, local signatures, and GADT constructor signatures (retaining only the documented Haskell98/GADT own-record-field pun exception with `-Wpun-uses` awareness), and ensure `GHC/Tc/Module.hs` brings all R21-scheduled term bindings into the `TcGblEnv` before any type signature referencing them is typechecked (eliminating `GHC-76329`).

---

### P1.9 — Over-Restrictive Dependent Pattern Matching, `case`, and Constructor `foreach` Gates (`#378` §4.5, §4.8, §4.9)
- **Authoritative Spec:**
  - `#378` §4.5 (lines 643–663): Any pattern match on a `foreach`-bound value in **checking mode** refines types on the RHS.
  - `#378` §4.9 (line 874): `data T where MkT :: forall (a :: Int) -> foreach (b :: Int) -> X a b -> T` (mixing existential `forall` and retained `foreach` in GADT constructors!).
- **Current Fork Divergence (`compiler/GHC/Tc/Errors/Ppr.hs:3766, 3826-3888, 3942-3955`, `GHC/Rename/HsType.hs:430-437`):**
  1. **Dependent `case` (`RetainedCasePatterns`, `Ppr.hs:3870`):** Rejects guards, wildcard/catch-all `_ ->` branches, nested constructor patterns (`S (S k) ->`), and TH quotes!
  2. **Dependent function equations (`RetainedConstructorCoverage` / `RetainedMatrixPattern`, `Ppr.hs:3838, 3862`):** Rejects guards on constructor equations and nested constructor patterns (`S (S k)`), and limits constructor matrices to 2 columns!
  3. **GADT constructors with `foreach` (`RetainedPackageDeclaration`, `RetainedConstructorResult`, `RetainedInvisibleField`, `GHC/Tc/Gen/HsType.hs:760-826`):**
     - Rejects invisible `foreach (n :: Nat).` fields (`RetainedInvisibleField`).
     - Rejects existential `forall` binders alongside `foreach` (`#378` line 874 `MkT :: forall (a :: Int) -> foreach (b :: Int) -> X a b -> T` fails with `RetainedPackageDeclaration`!).
     - Rejects non-`~` class dictionaries in retained constructors (`Mk :: foreach (n :: Nat) -> KnownNat n => ...`).
     - Rejects `foreach` inside `forall` in certain positions (`RetainedPolymorphicSignature`, `experimental_dependent_types.rst:599` — partially relaxed in M62/R03, verify all remaining gates!).
  4. **Signature context gate (`RetainedUnsupportedContext`, `GHC/Rename/HsType.hs:431-433`):** Rejects outer `foreach` in class default signatures (`DefaultSig`), `PatSynSig`, and top-level expression type signatures unless wrapped.
- **Required Alignment:**
  Lift these artificial gates incrementally via general case-tree / equation elaboration: desugar nested retained patterns (`S (S k)`) and guarded/catch-all branches into sequential single-constructor dependent cases, allow existential `forall` + class constraints + invisible `foreach .` in GADT constructors (`R41` + `R04`), and allow `foreach` in all complete signature contexts.

---

### P1.10 — Static Subset & Retained Application Inference Gaps (`#378` §4.1 & §4.7)
- **Authoritative Spec:**
  - `#378` §4.7 (lines 770–790): Constructors and applications (`f [True]`, `f (True : [])`, `f (CHole)`, `PStep (App l' m) ...`) are in the **initial Static Subset** of dependent applications!
  - `#378` §4.1: Type inference should instantiate ordinary polymorphic lemmas (`lamCong`, `appCong`) without manual `@` boilerplate when types are determined.
- **Current Fork Divergence (Active PLFH Reproducers!):**
  1. **Inline GADT constructors as `foreach` arguments (`R03_InlineGadtCtorForEachArg.hs`, `R04_ExistentialGadtRetainedArg.hs` — `GHC-83605`):**  
     Passing inline `(P.CHole :: P.UCtx Zero Zero)` to a `foreach` function, or passing a GADT constructor with existential type variables (`App l' m` where `App :: Term g (TArr a b) -> Term g a -> Term g b`) to `PStep (App l' m) (XiApp1 s)`, fails with `RetainedIndexExpression` (*"GHC cannot use this expression as a retained index"*), even though `App` is a plain constructor application in `#378`'s Static Subset!
  2. **Inline annotated list literals as `foreach` arguments (`PLFH-NOTES.md:839` — `GHC-83605`):**  
     `shuntAppend ([0, 1] :: [Nat]) ...` fails `GHC-83605`, whereas `let xs1 = [0, 1] :: [Nat] in shuntAppend xs1 ...` succeeds!
  3. **Untouchable meta-variables under retained applications (`R03_UntouchableRetainedCallPoly.hs` — `GHC-83865`):**  
     Inside a `foreach (c :: UCtx m n)` equation, `lamCong (comp c eq)` fails with *"untouchable inside the constraints: () bound by a retained application"* unless every type parameter `@n @(P.plug c t1) @(P.plug c t2)` is written manually! (Caused by `tcCaptureNativeEvidence` / implication level bumping in `GHC/Tc/Gen/App.hs` trapping ordinary outer unification variables when no existential/skolem GADT equalities were introduced.)
- **Required Alignment:**
  1. Admit annotated expressions `(e :: ty)`, indexed GADT constructors (`CHole`), and existential-type GADT constructors (`App l' m`, recovering the existential type arguments `@a @b` from the checked expression `App @g @a @b l' m`!) in `tcRetainedOperand` (`GHC/Tc/Gen/App.hs`).
  2. Do **not** bump `TcLevel` (or allow touchable promotion / `NoQL` candidate matching through type-family indices like `P.plug c t1`) when capturing expression-local retained constraints that introduce no local equality Givens (`R03_UntouchableRetainedCallPoly.hs`).

---

### P1.11 — Local `let`/`where` Transparency vs `#378` §4.6 Skolem Rule (`R04_ComposedIsoFields.hs`)
- **Authoritative Spec:**
  - `#378` §4.6 (lines 693–713): By default, each term variable used in a type is its own skolem; `#378` line 710 notes *"retaining the knowledge that `n = 5` when, say, the right-hand side of a `let` is in the Static Subset"* as a desirable extension.
- **Current Fork Divergence:**
  - The fork already implements Static-Subset `let` unfolding for arithmetic/constructor scalars (`experimental_dependent_types.rst:219-256`) and single-equation unguarded `where` (`GHC/Tc/Gen/Match.hs:2757`), **but**:
    1. Local non-recursive composite functions in `let`/`where` (`to x = h (f x)` in `R04_ComposedIsoFields.hs`) remain opaque skolems, so `from (to x)` cannot reduce to `g (k (h (f x)))`!
    2. Multi-clause functions' `where` blocks are excluded (`Match.hs:2748`).
    3. `experimental_dependent_types.rst:628-629` contradicts lines 219–256 by stating *"The checker does not equate a name with its definition."*
- **Required Alignment:**
  Extend local Static-Subset unfolding (`GHC/Tc/Gen/Bind.hs` / `HsType.hs`) to non-recursive local function compositions (`to x = h (f x)` / `to = h . f` where `h` and `f` are in-scope retained/promoted functions, fixing `R04_ComposedIsoFields.hs`), support non-guard-dependent `where` bindings across multi-clause matches, and reconcile the User's Guide documentation in `experimental_dependent_types.rst`.

---

### P1.12 — Existentials (`exists`) & Retained Sums (`some` / `GHC.Sigma`) Divergences from Proposal `#473` & Serokell `R04`
- **Authoritative Spec:**
  - Proposal `#473` §2.3–2.8: `exists tv_bndrs . ctype` & `exists tv_bndrs | ctype` (multi-binder telescopes including `{k}`), `GHC.Exists` exporting `Witness`, `(/\)`, `(/\#)`, `Pack`, `discardEvidence`, and `@Pack` visibility overrides in expressions and patterns.
  - Serokell `R04` (*Dependent sums*): `some (a :: A) | F a` (and by symmetry with `#473` §Motivation line 124 & `#378` §4.4, multi-binder telescopes `some (a :: A) (b :: B a) | C a b` and implicit dot form `some (a :: A). F a` + `GHC.Sigma.second`).
- **Current Fork Divergence:**
  1. `compiler/GHC/Parser.y:2304-2309`: `exists` only accepts a **single** `tv_bndr_no_braces` (`exists k (a :: k). ty` and `exists {k} (a :: k). ty` are **parse errors**!).
  2. `libraries/base/src/GHC/Exists.hs`: Only exports `pattern Pack` — missing `(/\)` (`exists a. Show a /\ a`), `@Pack` override syntax, `Witness`, and `discardEvidence`!
  3. `compiler/GHC/Parser.y:2319`: `some` only accepts `'some' '(' tyvar '::' ctype ')' '|' ctype` (single binder, mandatory parens/type, no multi-binder telescope, no implicit `.` form), and `GHC/Rename/HsType.hs:1230` fails with `RetainedSumContext` anywhere `GHC/Rename/Module.hs:346` didn't pre-lift it!
  4. `libraries/base/src/GHC/Sigma.hs`: Exports `Sigma(Pair)`, `first`, `withSigma`, **but lacks `second :: foreach (p :: Sigma a b) -> b (first p)`** (even though `docs/dependent-haskell/examples/packed-vector/DependentProjection.hs` already proves the compiler can typecheck `second`!).
- **Required Alignment:**
  1. Upgrade `GHC/Parser.y` so `exists` accepts full `tv_bndrs` (`exists k (a :: k). ty`, desugaring to nested `ExistsTy`s) and `some` accepts `foreach_bndrs` with `|` (and optional `.`).
  2. Lower `HsTypeSome` directly in `rnHsTyKi` / `tc_lhs_type` when not needing top-level capture family lifting (or generalize lifting so `some` works in any type position without `RetainedSumContext`).
  3. Export `second` (and `sndSigma` / `proj1` / `proj2` aliases) from `GHC.Sigma` in `base`!
  4. Implement `(/\)` (`MkCPair` dictionary pairing) and `@Pack` in `GHC.Exists` (`R22` / `#473`).

---

### P1.13 — Custom `FromNatural` (`GHC.TypeNats.Experimental`) vs `#364` / Serokell `R07` Overloaded Literals
- **Authoritative Spec:**
  - Serokell `R07` (*Promote literals*) + `#364`: Type-level numeric/string literals should overload via promoted `Num` (`fromInteger` / `fromNatural`) / `IsString` (`fromString`) identically to term-level literals, rather than requiring a parallel type-only conversion family!
- **Current Fork Divergence:**
  - `libraries/ghc-experimental/src/GHC/TypeNats/Experimental.hs` & `experimental_dependent_types.rst:140-165, 269-289`: Requires defining **both** `type instance FromNatural MyNat n = ...` (for type-level numerals) **and** a rigid 3-branch `instance Num MyNat` `fromInteger` (for term-level retained numerals), then checks that the two agree!
- **Required Alignment:**
  When a ground `Num T` instance in scope has a certified `fromInteger` (already recognized by `sourceLiteralWorkers` in `GHC/Tc/Gen/SourceTable.hs`!), automatically resolve type-level numerals at expected kind `T` using that certified `Num T` conversion **without** requiring a separate `FromNatural T` instance — while keeping `FromNatural` as a supported backward-compatible fallback!

---

## 4. Priority 2 — Unimplemented Proposal #378 & Sister-Proposal Features Mapped to Serokell Cards (`R01`–`R50`)

*(23 cards are already completed upstream; 4 cards — `R18`, `R35`, `R36`, `R41` — are qualified complete on this fork. Below are all remaining open/partial roadmap cards and their concrete remaining obligations.)*

| Card(s) | Feature & Authority (`#378` / Proposal) | What Remains Unimplemented on the Fork | Open PLFH Reproducers / Tests |
| :--- | :--- | :--- | :--- |
| **`R03`** | **Dependent products (`Π`-types)** (`#378` §4.4–4.9) | 1. `@` / `@pat` overrides for `foreach .` (P1.2).<br>2. Unannotated `foreach a ->` & wildcard `foreach (_ :: a) ->` (P1.3).<br>3. Inline indexed/existential GADT constructors as `foreach` args & untouchability fix (P1.10).<br>4. True `foreach (a :: Type) ->` phase-distinction & optional runtime `Type` pattern matching (`#378` lines 885–895) (P1.6).<br>5. General guards, catch-alls, nested patterns in dependent equations & `case` (P1.9).<br>6. Linear / multiplicity-polymorphic `foreach (x %1 :: a) ->` (`GraD`, `#378` line 620). | `R03_InlineGadtCtorForEachArg.hs`, `R03_UntouchableRetainedCallPoly.hs`, `R04_ExistentialGadtRetainedArg.hs`, `R03_FunctionColumnSplit.hs` (`-O2`) |
| **`R04`** | **Dependent sums (`Σ`-types)** (Serokell `R04`, `#378` §4.9) | 1. Export `second` from `GHC.Sigma`; multi-binder `some (x :: a) (y :: b x) \| c`; `some` in arbitrary type positions (P1.12).<br>2. Existential `forall` + class dicts + invisible `foreach .` in retained GADT constructors (P1.9).<br>3. Parameterized datatype dependent record updates (`R04-dependent-sums.md:483` currently restricts dependent record update to nullary datatypes).<br>4. Local composite function fields (`to x = h (f x)`). | `R04_ComposedIsoFields.hs`, `R04_ExistentialGadtRetainedArg.hs` |
| **`R05`** | **Dependent Core language** (`#378` §4.10, `DC`/`System DR`/`GraD`) | 1. **CRITICAL BUG:** Fix Desugar Core Lint cast placement on nested GADT `foreach` constructor applications (`($WFS y) |> co` vs `'FS (y |> co)` in `R05_NestedFinForEachCoreLint.hs`!).<br>2. Generalize `INLINE`/`INLINABLE` unfolding serialization and `-O2` Core-to-Core passes (`Specialise`, `FloatOut`, `Simplify`) for native `DepFunTy` bindings (currently gated in `experimental_dependent_types.rst:667-674`). | `R05_NestedFinForEachCoreLint.hs` |
| **`R07`** | **Promote literals** (`#364`, `#387`, `R07`) | Auto-bridge certified `Num` (`fromInteger`) and `IsString` (`fromString`) to type-level numerals/strings without manual `FromNatural` boilerplate (P1.13); negative/fractional literal diagnostics. | — |
| **`R10`** | **Promote unboxed types** (`#14180`, `R10`) | Extend beyond `Int#` / `Word#` fields to `ByteArray#` (`Text`, `Integer` `BN#`), unboxed tuples `(# a, b #)`, and target-width cross-compilation checks. | — |
| **`R11`** | **Homogeneous equality** (`#378` §4.10, `#17368`) | Complete homogeneous coercion binders (`a ~# b` carrying scoped kind coercion) across all GADT/pattern-match desugaring paths so mixed-kind endpoints never fail Core Lint. | (Previously hit in `R11_ExistentialNestedGadtCoreLint.hs`; keep auditing alongside `R05_NestedFinForEachCoreLint.hs`) |
| **`R12`** | **Promote classes** (`R12`, `R13`, `R24`) | Generalize promoted class method evaluation beyond concrete 1-parameter instances to multi-parameter classes, superclass dictionary projection, and recursive instance dictionaries (e.g. `Eq [a]`, `Ord (a, b)`). | — |
| **`R13`, `R15`, `R16`** | **Function promotion, Closed & Open evaluators** (`#378`, `#509`, `R13`–`R16`) | 1. Default-on source equations without `-fwrite-source-equations` (P1.5).<br>2. Same-module open equations for `plug` & `substSingle` (P1.5).<br>3. Large eliminations `Val -> Type` (P1.7).<br>4. `-O2` `base` list-fusion rule compatibility without `-fno-enable-rewrite-rules` (P1.5).<br>5. Open evaluation with abstract higher-order renaming/callback variables (`R.rename r m` in `R16_AbstractRenamingInType.hs`).<br>6. Same-module closed evaluation (`R21-dependency-analysis.md:197` currently requires imported `.o` for interpreter-backed closed calls). | `R13_LargeElimTermToType.hs`, `R13_SameModulePlugOpenEq.hs`, `R21_SameModuleSubstSingleOpenEq.hs`, `R16_AbstractRenamingInType.hs`, `R13_RevOpenEq.hs` (`-O2`), `R13_ShuntAccumOpenEq.hs` (`-O2`) |
| **`R14`** | **Data family promotion** (`#456`, `R14`) | Promote data-family instance constructors that carry class constraints or existential binders. | — |
| **`R17`, `R19`** | **Type-level lambdas & cases** (`R17`, `R19`) | 1. Allow capturing unmatchable `%U ->` variables inside inline type lambdas (`@(\z -> p z -> p x)` in `R17_TypeLambdaCaptureUnmatchable.hs`, `GHC-27185`!).<br>2. Support inline type lambdas & cases in arbitrary type contexts (not only top-level synonyms / lifted signatures) and in typed TH quotes `[|| ... ||]`.<br>3. Eta-equivalence / forwarding between `\x -> F x` and unsaturated `F` (`ROADMAP.md:193`). | `R17_TypeLambdaCaptureUnmatchable.hs` |
| **`R21`** | **Dependency analysis** (`R21`, `#378` §4.3.2) | Unify R21 topological scheduling with closure/GADT open-equation synthesis so *any* function usable cross-module is usable earlier or later in the **same module** (`plug`, `substSingle`, local class methods). | `R13_SameModulePlugOpenEq.hs`, `R21_SameModuleSubstSingleOpenEq.hs` |
| **`R22`** | **First-class existentials** (`#473`, `R22`) | Multi-binder `exists tv_bndrs .` / `|`, `-XExistentialTypes` flag, `GHC.Exists` `(/\)` constraint-pair operator, `@Pack` explicit witness override in expressions & patterns, `Witness` type projection, and `-O2` support for existential packages. | — |
| **`R23`** | **Unsaturated type families** (`#242`, `#370`, `R23`) | 1. Matchable-to-unmatchable adaptation / subtyping (`EqP x :: a %M -> Type` instantiating `p :: a %U -> Type` in `R23_MatchabilityInLeibniz.hs`!).<br>2. Fix the sole failing focused test `deriving-via-compile` (`MTrans` matchability defaulting on stock transformer kind synonyms, `HANDOFF.md:36`).<br>3. Revisit `#242`'s `%M` default on higher-order type variables (`r :: forall x. Lookup g x -> Lookup d x`) when passed to promoted functions (`R16_AbstractRenamingInType.hs`). | `R23_MatchabilityInLeibniz.hs`, `R16_AbstractRenamingInType.hs`, `deriving-via-compile` (`MTrans`) |
| **`R24`** | **Constrained type families** (`#177`, `R24`) | Standalone constrained kind signatures for open/closed type families (`type F :: Show a => a -> Symbol`) and first-class Core domain evidence on partial family applications. | — |
| **`R27`** | **Deprecate CUSKs** (`#54`, `R27`) | Fork intentionally retains H98/H2010 defaults + `-Wcusks` migration warning (`ROADMAP.md:176`); keep as-is unless `GHC2024`/DH opt-in disables `CUSKs` (note: `StandaloneKindSignatures` in `DependentTypes` already turns `Off LangExt.CUSKs`!). | — |
| **`R34`** | **Syntactic unification** (`#378` §4.3.3, `#281`, `#25121`) | 1. Add `foreach`/`exists`/`some` to `infixexp2` & `T2T` (P1.4).<br>2. Add `data` expression/type escape herald (`#378` §4.3.4).<br>3. Longer-term upstream refactoring: unify `HsExpr` and `HsType` ASTs (`#25121`). | — |
| **`R01`, `R02`, `R06`, `R20`** | **Aggregate milestone cards** | Close automatically once their constituent child cards above pass full two-stage + Showcase + PLFH qualification. | — |

---

## 5. Priority 3 — Other GHC Proposals + Agda / PLFA-Inspired Ergonomics Beyond `#378`

Once Priority 1 divergences and core Priority 2 blockers are resolved, these Tier 2 / Tier 4 ergonomics dramatically reduce ceremony in dependently typed Haskell (as measured across all 25 chapters of PLFH):

1. **Multi-Binder Shared-Type Telescopes (`foreach (m n p :: Nat) ->` and `forall (m n p :: Nat).`)** *(Agda telescopes; `PLFH-NOTES.md:1601`)*:  
   - Standard GHC requires repeating parentheses and types: `foreach (m :: Nat) (n :: Nat) (p :: Nat) ->`. Allowing space-separated binders sharing one `:: Type` annotation `(m n p :: Nat)` (and `{m n p :: Nat}` for invisible binders) in `tv_bndr` / `foreach_bndrs` cuts signature verbosity by 40–60% in arithmetic and metatheory proofs while being 100% unambiguous in `GHC/Parser.y` (`'(' tyvar_wc tyvar_wcs1 '::' kind ')'`, desugared in the parser to `[HsTvb v kind | v <- vs]`).
2. **Local `type` Synonyms in `let`/`where` & `where` Clauses on Type Signatures (`Proposal #523`, `-XExtendedLet`)**:  
   - Allow `let type Env = ... in ...` / `where type Sub = ...` in terms, and `f :: ... where { type T = ... }` on type and data declarations (`#523` §1.2 & §2.2), complementing our completed `R18` `let ... in` in types!
3. **Dependent Record Dot Syntax (`Proposal #282` / `#583` + Agda Dependent Records, `R04`)**:  
   - Our fork already generates top-level dependent record selectors (`cells :: foreach (r :: Columns a) -> Vec a (storedLength r)`, `R04-dependent-sums.md:476`). Extending `OverloadedRecordDot` (`r.cells`) to resolve dependent selectors when `HasField` cannot express the value-dependent result type gives clean Agda-style projection syntax without field-name clashes!
4. **GADT-to-`foreach` Coverage Refinement & Absurd Empty-Case Synthesis (`Lower Your Guards` ICFP 2020 + Agda `()`)**:  
   - When matching an ordinary GADT argument refines a `foreach` index to an uninhabited constructor combination (or makes remaining `foreach` constructors impossible), let PMC (`GHC.HsToCore.Pmc`) recognize exhaustiveness without requiring unreachable boilerplate clauses, and consider `\cases` / empty-pattern ergonomics for absurd branches.
5. **Agda-Style `with` View / Intermediate Dependent Scrutinee Ergonomics**:  
   - In Agda/PLFA (`Decidable`, `Inference`, `Untyped`), `with f x` abstracts an intermediate call across goal and context types. In our fork, `case f x of ...` already supports remembered call equality (`M47`, `Showcase.hs:610-620`) for unary/constructed calls — generalizing dependent `case expr of` motive & context refinement to arbitrary promoted/retained calls gives idiomatic Haskell `case` the power of Agda's `with ... in eq` without inventing non-Haskell syntax!
6. **Operator Sections in Types & Required Type Arguments (`cong (+ p)`, `(+ 1)`)**:  
   - Support left/right operator sections `(op e)` and `(e op)` in `T2T` and type syntax (desugaring `(`op` y)` to `\x -> x `op` y`), matching PLFA's `cong (_+ p)`.
7. **`dependent-base` / `HaskLib` Surface Consolidation (`ROADMAP.md:314`, `PLFH-NOTES.md:1600`)**:  
   - Re-export `Matchability(..), M, U` (from `GHC.Matchability`), `Sigma(..), first, second, withSigma` (from `GHC.Sigma`), `Pack, (/\), Witness` (from `GHC.Exists`), `Nat`, `Fin`, `Vec`, `(:~:)(..)`, `(:~~:)(..)`, `cong`, `sym`, `trans`, `rewrite`, and certified total list operations (`foldr`, `map`, `(++)`, `length`) from a clean public `HaskLib` / `Data.Dependent.*` surface so users never import `GHC.Internal.*` or `GHC.Exts`.
8. **Conservative Advisory Totality Checker (`ROADMAP.md:315, 332–341`, `#378` §4.10)**:  
   - Opt-in warning/pragma (`-Wincomplete-totality` or `{-# TOTAL #-}`, strictly advisory per `#378` §4.10) checking exhaustive patterns (`PMC`), strict positivity, and structural descent on the zonked AST call graph.

---

## 6. Concrete Wave 4+ Implementation Tracks (Non-Overlapping Module Ownership)

To maximize parallel velocity without merge conflicts, Wave 4+ is partitioned into **five orthogonal compiler module tracks**. Every track preserves backward compatibility (`ExperimentalDependentTypes`, `@{...}`, `-fwrite-source-equations`, `FromNatural`) while making `#378` canonical.

### Track A — Surface Syntax, Extension Bundle & Telescope Ergonomics (P1.1, P1.3, P1.4, P3.1)
- **Owned Modules:**  
  `libraries/ghc-boot-th/GHC/LanguageExtensions/Type.hs`, `compiler/GHC/Driver/Flags.hs`, `compiler/GHC/Parser/Lexer.x`, `compiler/GHC/Parser.y`, `compiler/GHC/Parser/PostProcess.hs`, `docs/users_guide/exts/experimental_dependent_types.rst`.
- **Deliverables:**
  1. Add `-XDependentTypes` (implying the full DH extension bundle + `ExperimentalDependentTypes` alias), `-XExistentialTypes` (`#473`), and `-XExtendedLet` (`#523`); fix `experimental_dependent_types.rst:686`.
  2. Add Unicode `∏` (`U+220F`) for `foreach` under `-XUnicodeSyntax`.
  3. Allow `{x}` / `{x :: a}` specificity braces on `foreach .` (`GHC/Parser.y:2578`), multi-binder `exists tv_bndrs .` / `|` (`GHC/Parser.y:2304`), multi-binder `some` telescopes (`GHC/Parser.y:2319`), and shared-annotation binder groups `(x y z :: a)` / `{x y z :: a}` (P3.1).
  4. Add `foreach`, `exists`, and `some` to `infixexp2` (`types-in-terms`, `#378` §4.7 / P1.4).

### Track B — Quantifier Semantics, `@` Visibility Unification, `foreach (a :: Type)` & Application Inference (P1.2, P1.3, P1.6, P1.10)
- **Owned Modules:**  
  `compiler/GHC/Rename/Bind.hs`, `compiler/GHC/Tc/Gen/App.hs`, `compiler/GHC/Tc/Gen/Head.hs`, `compiler/GHC/Tc/Gen/Expr.hs`, `compiler/GHC/Tc/Gen/Match.hs`, `compiler/GHC/Tc/Gen/Pat.hs`, `compiler/GHC/Tc/Utils/Instantiate.hs`.
- **Deliverables:**
  1. **Canonical `@` for `foreach .` (P1.2):** Support `f @arg` at call sites and `f @n` / `f @Zero` / `f @(Succ k)` (`InvisPat`) at definition sites for `DepFunTy DepFunInvisible` (retaining `f @{arg}` as a compatibility alias).
  2. **Unannotated `foreach a ->` & wildcard `foreach (_ :: a) ->` (P1.3):** Infer omitted binder domain types from body usage and permit `_` wildcard binders.
  3. **True `foreach (a :: Type)` (P1.6):** Keep `a` in scope as a type variable (`ex5 a = the a undefined`, `x :: a`) alongside `Typeable a` evidence, and accept unadorned `lookupColumn Char ...` at call sites via `T2T`.
  4. **Static Subset & Touchability Fixes (P1.10):** Fix `R03_InlineGadtCtorForEachArg.hs`, `R04_ExistentialGadtRetainedArg.hs`, annotated list literals `([0,1] :: [Nat])`, and `R03_UntouchableRetainedCallPoly.hs`!

### Track C — Unified Function Promotion, Same-Module Open Equations, Large Eliminations & `-O2` Rules (P1.5, P1.7, P1.13)
- **Owned Modules:**  
  `compiler/GHC/Tc/Module.hs`, `compiler/GHC/Tc/Gen/SourceTable.hs`, `compiler/GHC/Tc/Gen/OpenTypeEval.hs`, `compiler/GHC/Driver/Main/Passes.hs`, `compiler/GHC/Core/SourceClosure.hs` (or closure-table helpers).
- **Deliverables:**
  1. **Kill the `-fwrite-source-equations` tax & same-module split (P1.5):** Default source-equation publication to `On` under `DependentTypes`/`ExperimentalDependentTypes`, synthesize `plug` and `substSingle` tables during R21 declaration scheduling in `GHC/Tc/Module.hs`, and pass both `R13_SameModulePlugOpenEq.hs` and `R21_SameModuleSubstSingleOpenEq.hs`!
  2. **Large Eliminations `Val -> Type` (P1.7):** Support functions returning `Type` (`valPred :: Val -> Type`) via `T2T` RHS checking + `SourceTable` closed-family synthesis, passing `R13_LargeElimTermToType.hs` (eliminating the last `type family` in PLFH!).
  3. **`-O2` Standard List Fusion Whitelist (P1.5):** Certify `base` list fusion (`build`/`augment`/`map`/`(++)`) for spine-observing open equations so `R03_FunctionColumnSplit.hs`, `R13_ShuntAccumOpenEq.hs`, and `R13_RevOpenEq.hs` pass at default `-O2` without `-fno-enable-rewrite-rules`!
  4. **Automatic `Num` Literal Promotion (P1.13):** Use certified `Num` `fromInteger` instances directly for type-level numerals when no explicit `FromNatural` instance is written.

### Track D — Existentials (`#473`), Retained Sums (`some` / `GHC.Sigma`), Local Composition & Dependent Records (P1.9, P1.11, P1.12)
- **Owned Modules:**  
  `libraries/base/src/GHC/Sigma.hs`, `libraries/base/src/GHC/Exists.hs`, `compiler/GHC/Rename/HsType.hs`, `compiler/GHC/Tc/Gen/Bind.hs`, `compiler/GHC/Tc/TyCl.hs`, `compiler/GHC/Tc/TyCl/Utils.hs`, `docs/dependent-haskell/hasklib/`.
- **Deliverables:**
  1. **`GHC.Sigma` & `some` everywhere (P1.12):** Export `second :: foreach (p :: Sigma a b) -> b (first p)` from `GHC.Sigma`, lower uncaptured `HsTypeSome` directly without `RetainedSumContext` failures, and support parameterized dependent record updates.
  2. **Retained GADT Constructors (`#378` §4.9, P1.9):** Allow existential `forall`, class constraints, and invisible `foreach .` in retained GADT constructors (`MkT :: forall a -> foreach b -> X a b -> T`).
  3. **Local Composite Functions (P1.11):** Support Static-Subset unfolding for non-recursive local composite functions (`to x = h (f x)`), passing `R04_ComposedIsoFields.hs`.
  4. **`#473` `GHC.Exists` Completion (P1.12 / `R22`):** Add `(/\)` constraint-pair type/desugaring and `@Pack` explicit witness overrides, plus public `HaskLib` re-exports.

### Track E — Dependent Core Lint Fix (`R05`), Matchability Subtyping (`R23`) & Type-Lambda `%U` Capture (`R17`)
- **Owned Modules:**  
  `compiler/GHC/HsToCore/Expr.hs`, `compiler/GHC/HsToCore/Pmc/`, `compiler/GHC/Core/Coercion.hs`, `compiler/GHC/Rename/Module.hs` (lambda lifter), `compiler/GHC/Tc/Solver/Equality.hs`, `compiler/GHC/Tc/Gen/HsType.hs` (matchability/lambda checking).
- **Deliverables:**
  1. **URGENT Core Lint Fix (`R05_NestedFinForEachCoreLint.hs`):** Push constructor wrapper casts (`($WFS y) |> Sub (Fin ('Succ co))`) inward to `WFS (y |> Fin co)` during retained argument desugaring/elaboration so the runtime expression and promoted `'FS (y |> Fin co)` type index match in Core Lint!
  2. **Type-Lambda `%U` Capture (`R17_TypeLambdaCaptureUnmatchable.hs`):** Preserve `%U ->` matchability annotations on captured scoped type variables in `GHC/Rename/Module.hs` (`liftLambdaRhs`), unlocking pure impredicative Leibniz symmetry `xy @(\z -> p z -> p x) (\px -> px)`!
  3. **Matchable-to-Unmatchable Subtyping & `MTrans` (`R23_MatchabilityInLeibniz.hs`, `R16_AbstractRenamingInType.hs`, `deriving-via-compile`):** Allow matchable `%M ->` constructors (`EqP x`) to eta-adapt to expected `%U ->` callback kinds (`\y -> EqP x y`), support abstract renaming callbacks in open evaluation (`R16_AbstractRenamingInType.hs`), and resolve the `MTrans` `deriving-via-compile` diagnostic (`138/138` focused tests green!).

---

### Suggested Wave Sequencing

- **Wave 4 (Immediate High-Impact Alignment + All 14 Open PLFH Reproducers):**
  - **Lane 1 (Track E):** Fix `R05_NestedFinForEachCoreLint.hs` (Core Lint bug!), `R17_TypeLambdaCaptureUnmatchable.hs`, `R23_MatchabilityInLeibniz.hs`, and `deriving-via-compile` (`MTrans`).
  - **Lane 2 (Track C):** Default-on source equations (no `-fwrite-source-equations` needed), fix `R13_SameModulePlugOpenEq.hs` & `R21_SameModuleSubstSingleOpenEq.hs`, implement large eliminations (`R13_LargeElimTermToType.hs`), and whitelist `-O2` list fusion (`R03_FunctionColumnSplit.hs`, `R13_ShuntAccumOpenEq.hs`, `R13_RevOpenEq.hs`).
  - **Lane 3 (Track B):** Canonical `@` / `@pat` for `foreach .` (keeping `@{...}` alias), unannotated `foreach a ->` & wildcard `foreach (_ :: a) ->`, true `foreach (a :: Type)` (`#378` §4.9), and fix `R03_InlineGadtCtorForEachArg.hs`, `R04_ExistentialGadtRetainedArg.hs`, & `R03_UntouchableRetainedCallPoly.hs`.
  - **Lane 4 (Track A + Track D):** Add `-XDependentTypes` umbrella bundle + `∏` + `(x y z :: a)` telescopes + `foreach`/`exists`/`some` in `infixexp2`; export `GHC.Sigma.second`; fix `R04_ComposedIsoFields.hs`.
- **Wave 5 (Deep `#378` / `#473` / `#523` Completion & `Showcase` / `PLFH` Canonical Migration):**
  - Generalize dependent pattern/case matrices (guards, nested patterns, catch-alls, GADT existential+`foreach` constructors), complete `#473` (`/\`, `@Pack`, `Witness`, `-XExistentialTypes`) and `#523` (`-XExtendedLet`), generalize native `-O2` unfolding serialization (`R05`), and migrate `Showcase.hs`, `HaskLib`, and `PLFH` from legacy fork spellings (`@{...}`, `-fwrite-source-equations`, `-fno-enable-rewrite-rules`, 15-pragma headers, `LogVLam` `type family`) to clean canonical `#378` Dependent Haskell!
