# Dependent Core Specification

This document specifies GHC's typed dependent Core representation, matchability annotations, coercion and Lint invariants, interface encoding, source-equation certification, and STG erasure.

## 1. Core Type Syntax and Matchability

`GHC.Core.TyCo.Rep.Type` extends System FC with four dependent constructors and explicit matchability metadata on functions and quantifiers:

```text
DepFunTy vis b result     -- Dependent function type (foreach -> / foreach .)
ExistsTy tv body          -- Erased first-class existential type (exists . / |)
TermVarTy b               -- Rigid runtime term-variable index
TermAppTy f arg           -- Scoped runtime function-application index
FunTy af mult arg res     -- Arrow carrying Matchability in FunTyFlag
ForAllTy ma bndr body     -- Quantifier carrying Matchability (M or U)
```

### 1.1 Retained Function Binders (`DepFunTy` and `TermVarTy`)

- In `DepFunTy vis b result`, `b` is a local non-coercion, non-join value `Id` whose type has kind `Type` (or lifted value representation) and whose multiplicity is `Many`, `One`, or a multiplicity variable `m`.
- `vis` is `DepFunVisible` (`foreach (x :: a) -> b`) or `DepFunInvisible` (`foreach (x :: a). b`).
- `TermVarTy b` embeds a lexical runtime `Id` `b` into a type at kind `varType b`, preserving `b`'s `Unique` across occurrences. Constructor indices (`'True`, `'Zero`, `'Succ n`) use standard `DataKinds` promotion (`TyConApp`), so `DataKinds` constructors and `TermVarTy` variables compose directly inside type expressions such as `'Succ (TermVarTy n)`.
- `mkDepFunTy` canonicalizes a visible `DepFunVisible` binder with multiplicity `Many` that does not occur free in `result` to an ordinary unmatchable arrow `FunTy`. Invisible `DepFunInvisible` binders and non-`Many` binders preserve `DepFunTy` so visibility coercions and linear multiplicity checks retain binder structure.
- Core value lambdas `\ (b :: a) -> body` have visible `DepFunTy` types; a representational visibility coercion converts between `DepFunVisible` and `DepFunInvisible` without allocating an extra runtime lambda.

### 1.2 Erased Existential Types (`ExistsTy`)

- `ExistsTy tv body` binds an erased type or coercion variable `tv` existentially over `body` (`GHC Proposal #473`).
- `ExistsTy` introduces zero runtime wrapper boxes: packing casts or wraps an expression at its witness instantiation, and elimination opens `tv` as a fresh skolem in scope over the continuation while preserving call-by-need laziness on the payload.

### 1.3 Scoped Runtime Application Indices (`TermAppTy`)

- `TermAppTy f arg` represents a stuck runtime call index `f arg` in a type when `f` is an in-scope function `Id` (for example a `foreach (f :: a -> b) ->` parameter or function-valued constructor field) applied to an index `arg`.
- `TermAppTy` has kind equal to the result type of `f` instantiated at `arg`, remains opaque to constructor discrimination until `f` is instantiated to a known function with certified equations, and compares nominally by head and argument.

### 1.4 Arrow and Quantifier Matchability (`Matchability`)

- `Matchability` (`GHC.Matchability`) has constructors `Matchable` (`M`) and `Unmatchable` (`U`) (`GHC Proposal #242`).
- Ordinary value arrows default to `U`; datatype and class constructor kind arrows default to `M`; type synonym and type family arity prefixes use `U` while preserving the declared result kind.
- `ForAllTy ma bndr body` stores `ma :: Matchability` outside `bndr`'s scope. Runtime type and coercion lambdas require `U`; generative datatype and class quantifiers use `M`.
- Decomposition of `f a ~ g b` into `f ~ g` and `a ~ b` (`LRCo`, injective decomposition, and solver decomposition) requires matchability `M` on the arrow kind. Forward congruence (`AppCo`) holds for both `M` and `U`.

## 2. Structural Index Domains and Retained Constructor Fields

### 2.1 Admissible Index Domains

- `isNativeIndexType` validates lifted algebraic datatypes, GADT index domains, closed parameterized containers (`[a]`, `Maybe a`, `Either a b`, `(a, b)`, `Vec a n`), function-domain parameters (`a -> b`), and `Type` runtime witnesses (`Typeable` evidence).
- Recursive datatypes require regular parameter occurrences along recursive cycles; newtypes and unboxed types are rejected as `foreach` binder domains unless promoted via explicit primitive field rules (`Int#`, `Word#`, `Char#`, `(# a, b #)`).
- For imported declarations, `NativeDomainShape` (`GHC.Core.TyCon`) provides a lazy precheck that detects alias-only synonym cycles and validates domain shapes before allocating local `Id`s during interface decoding.

### 2.2 Retained Constructor Fields (`DataCon`)

- Algebraic GADT constructors can mix universal and existential `forall` binders, class and equality constraints, and visible or invisible `foreach` fields (`MkT :: forall a -> foreach (n :: Nat) -> Vec a n -> T`).
- `DataCon` stores optional retained field binder names (`ifConArgBinders :: [Maybe IfLclName]` in interfaces) alongside `dcOrigArgTys` rather than placing runtime `Id`s in `dcExTyCoVars`.
- Each retained field binds its runtime `Id` over subsequent field types in source order and occupies one ordinary lazy runtime slot in the heap object. The constructor result type cannot contain free field `Id`s.

## 3. Dependent Coercions, Roles, and Normalization

### 3.1 `DepFunCo` and Homogeneous GADT Coercions

- `DepFunCo` witnesses equality between two `DepFunTy` types over a shared runtime binder `b`, carrying nominal domain consistency and a scoped body coercion at the parent role (Nominal or Representational).
- Nominal equality can weaken to representational equality (`SubCo`), whereas constructor discrimination and argument congruence in dependent indices require Nominal role (`System DR`).
- GADT pattern matching and constructor wrapper elaboration bind homogeneous coercion variables (`lhs ~# rhs` paired with explicit kind coercions when endpoints have distinct kinds) so nested matches remain well-kinded under Core Lint.

### 3.2 Scoped Family Normalization Beneath `DepFunTy`

- The constraint solver and `tcRewriteFamilyAxioms` reduce top-level closed and open type-family axioms beneath `DepFunTy` binders at Nominal and Representational roles (`mkDepFunRedn`), leaving stuck applications neutral.
- Normalization beneath a `DepFunTy` binder excludes ambient local Given equalities and solver plugins so rewriting never captures an out-of-scope coercion variable, and never performs function eta-expansion (`seq (\x -> bottom) True` vs `seq bottom True`).
- After rewriting both endpoints, the canonicalizer checks alpha-equivalence of `DepFunTy`, `ExistsTy`, `TermVarTy`, and `TermAppTy` by reflexivity.

## 4. Dependent Pattern Matching, Case Evidence, and Core Lint

### 4.1 Branch-Local Refinement Coercions

- Core `Case scrut case_bndr res_ty alts` keeps `res_ty` outside `case_bndr` and alternative binder scopes.
- When a `DataAlt` refines a retained index (such as `Var b` or a checked `TermAppTy` scrutinee), the alternative binds the constructor's real runtime fields `x_1 .. x_k` followed by a trailing virtual nominal coercion binder:
  ```text
  Alt (DataAlt S) [k :: Nat, cv :: TermVarTy n ~# 'S (TermVarTy k)] rhs
  ```
- Each field `x_i` enters scope before `cv` is kind-checked, and `cv` scopes only over `rhs`.
- Proof-bearing `Case` expressions must have exhaustive alternatives (all constructors of the domain, after refuting GADT-impossible constructors, or a valid `DEFAULT` branch) so contradictory branch evidence is guarded by strict runtime case evaluation.
- Runtime `Type` pattern matching on `foreach (a :: Type) ->` lowers unadorned type branches (`Int -> ...`, `[Char] -> ...`, `_ -> ...`) against the runtime `TypeRep` witness and introduces local nominal type equality in each matched branch.

### 4.2 Dependent Core Lint and Multiplicity Usage

- Core Lint (`GHC.Core.Lint`) checks lexical `Id` scope for every `TermVarTy` and `TermAppTy`, validates `DepFunTy` application substitution `result[b := arg_index]`, verifies `ExistsTy` packing and opening, checks `Matchability` proofs in `FunCo` and `ForAllCo`, and enforces exhaustive coverage on proof-bearing alternatives.
- Following graded dependent type discipline (`GraD`), type-level index and coercion occurrences of a `DepFunTy` binder consume `0` runtime multiplicity, while term-level occurrences must satisfy the binder's declared multiplicity (`%1`, `%m`, or `Many`).

## 5. Simplifier, Core-to-Core Pipeline, and Plugins

- `GHC.Core.Dependent` classifies native dependent Core forms across `ScanPurpose` modes (`DetectNative`, `CheckGentle`, `CheckUserRules`, `CheckDepFunUnfolding`, `CheckErasedTypes`).
- Modules containing native dependent Core run `occurAnalyseNativeExpr` (which retains local bindings referenced in `TermVarTy`, `TermAppTy`, coercions, case result types, or stable unfoldings before dropping dead `NonRec` bindings) and `simpleOptNativeProgram` (performing capture-safe dependent beta-reduction on admissible variable/constructor indices and known-constructor case reduction while preserving lazy `let` sharing and rejecting general function eta-reduction).
- Validated `INLINE` and `INLINABLE` `DepFunTy` unfolding templates (`supportsNativeDepFunUnfolding`) serialize across `.hi` interfaces and specialize at call sites.
- Standard GHC plugins (`sourcePlugin`, `renamerPlugin`, `splicePlugin`, `tcPlugin`, `defaultingPlugin`, `holeFitPlugin`, and `installCoreToDos` Core-to-Core passes) run under Dependent Core Lint prior to post-optimization source-equation verification; only active `latePlugin` hooks, profiling cost-centre insertion, and the JavaScript backend reject native dependent Core.

## 6. Source Equations, Closures, and Compile-Time Evaluation

- When `-fwrite-source-equations` is active (on by default under `-XDependentTypes`), GHC certifies structural functions and closure-capturing higher-order traversals into closed type families (`SourceTable` `T:<name>` and defunctionalized `CheckedClosureSourceTable` families in `GHC.Tc.Gen.SourceTable` and `GHC.Tc.Gen.SourceClosure`).
- Certification runs in topological SCC order during module typechecking so later declarations in the same module can reduce earlier functions in types, and re-verifies post-optimization Core (`matchesFinalSourceTable`) before writing `.hi` interfaces (including under `-fno-code -fwrite-interface`), tracking rewrite-rule dependencies per function.
- Functions returning `Data.Kind.Type` (`f :: D -> Type`) elaborate as large eliminations whose `type` right-hand sides populate `T:f :: D -> Type`.
- Closed ground calls (`GHC.Tc.Gen.Splice`) execute via bytecode in the target interpreter when no open variables appear, and open symbolic calls reduce through bounded lazy WHNF readback (`GHC.Core.OpenWhnf` and `GHC.Tc.Gen.OpenTypeEval`).
- When at least one `TcPlugin` is active, uncertified open term calls `f args` elaborate to deterministic equationless closed type families (`U:<occ>`) instead of failing immediately, allowing solver plugins to test, rewrite, or refute open equalities.

## 7. Interface Serialization (`.hi` and `.hie`)

- `IfaceType` encodes `IfaceDepFunTy` (visibility, `IfaceIdBndr`, body), `IfaceExistsTy`, `IfaceTermVar` (`IfLclName`), `IfaceTermAppTy`, and arrow/quantifier `Matchability` annotations without writing raw `Unique`s to disk.
- Tidying assigns distinct local `OccName`s to nested `DepFunTy` binders before serialization; `IfaceToCore` allocates fresh local `Id`s on import, reconstructs virtual case-branch coercion binders from the scrutinee and `DataCon`, and runs Dependent Core Lint inside each imported native unfolding's `forkM` thunk.
- Interface fingerprints include `Matchability` metadata, retained constructor field binders, and published `SourceTable` axioms so downstream consumers recompile automatically when equations or signatures change.
- With `-fwrite-ide-info`, `.hie` files record retained binder visibilities and runtime index occurrences.

## 8. CorePrep, Erasure, and STG/Bytecode Lowering

- `CoreToStg.Prep` preserves Dependent Core typing through ANF conversion (`cpCloneBndr` updates `TermVarTy` references whenever a runtime `Id` is cloned, `mkEtaWW` substitutes fresh eta binders into dependent codomains, and computed retained arguments bind lazy ANF names paired with scoped nominal transport equalities). Core Lint runs both before and after `CorePrep`.
- `CoreToStg` erases types, casts, `ExistsTy` witnesses, and trailing virtual alternative coercion binders while preserving real constructor fields, ordinary zero-width coercion-token arguments (`Note [Coercion tokens]` in `GHC.CoreToStg`), strict case discrimination, and lazy runtime thunks.
- STG Lint (`lintStgAppReps`) checks runtime representation compatibility across erased `DepFunTy` spines before native code generation (Cmm/NCG/LLVM) or GHCi bytecode compilation.

## 9. Historical Provenance and References

The Core binder representation builds on Richard Eisenberg's 2014 `nokinds-pi` prototype branch (`b64ef80f8002727422c05092f429c38d0527ed0b`), extended to modern GHC with explicit `TermVarTy`/`TermAppTy` indices, `ExistsTy`, `Matchability`, call-by-need ANF lowering, and certified source equations:

- Richard A. Eisenberg, *Dependent Types in Haskell: Theory and Practice*, PhD dissertation, University of Pennsylvania, 2016.
- Stephanie Weirich, Antoine Voizard, Pedro Henrique Azevedo de Amorim, and Richard A. Eisenberg, *A Specification for Dependent Types in Haskell*, Proc. ACM Program. Lang. 1 (ICFP), Article 31, 2017.
- Stephanie Weirich, Pritam Choudhury, Antoine Voizard, and Richard A. Eisenberg, *A Role for Dependent Types in Haskell*, Proc. ACM Program. Lang. 3 (ICFP), Article 101, 2019.
- Csongor Kiss, Tony Field, Susan Eisenbach, and Simon Peyton Jones, *Higher-Order Type-Level Programming in Haskell*, Proc. ACM Program. Lang. 3 (ICFP), Article 102, 2019.
- Pritam Choudhury, Harley Eades III, Richard A. Eisenberg, and Stephanie Weirich, *A Graded Dependent Type System with a Usage-Aware Semantics*, Proc. ACM Program. Lang. 5 (POPL), Article 50, 2021.
- Richard A. Eisenberg, Guillaume Duboc, Stephanie Weirich, and Daniel Lee, *An Existential Crisis Resolved: Type Inference for First-Class Existential Types*, Proc. ACM Program. Lang. 5 (ICFP), Article 64, 2021.
- Paulette Koronkevich, Ramon Rakow, Amal Ahmed, and William J. Bowman, *ANF Preserves Dependent Types up to Extensional Equality*, JFP 32, e12, 2022.
