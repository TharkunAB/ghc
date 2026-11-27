-- | Detect native dependent types in Core and its metadata.
module GHC.Core.Dependent (
    hasNativeDependentProgram,
    supportsNativeGentle,
    supportsNativeRuleBinds,
    supportsNativeUserUnfolding,
    supportsNativeDepFunUnfolding,
    supportsScopedDictionaryCore,
    hasNativeDepFunType,
    hasRetainedRuleHead, supportsLocalUserRule, supportsErasedUserRule,
    supportsImportedErasedRule, supportsImportedUserRule,
    supportsQuickLookType,
    hasNativeDependentExpr,
    hasExistentialExpr,
    hasNativeDependentExprRhs,
    hasNativeDependentUnfolding,
    isRetainedVanillaDefaultMethod,
    rejectNativeDependentProgram,
    rejectNativeDependentExpr,
    hasScopedDictionaryProgram, hasScopedDictionaryExpr, hasScopedDictionaryType,
    isScopedSelectorReceiver, rejectScopedDictionaryProgram, rejectScopedDictionaryExpr,
) where

import GHC.Prelude

import GHC.Builtin.WiredIn.Types (unmatchableDataConTy)
import GHC.Core
import GHC.Core.FVs (orphNamesOfExprs)
import GHC.Core.Class (classOpItems, classTyCon)
import GHC.Core.Predicate (getClassPredTys_maybe, mkClassPred)
import GHC.Core.Type (coreFullView, isNativeRetainedBinderType, isNativeIndexFunctionId
                     , isNativeDepFunctionId, isNativeNamedOpaqueFunctionId
                     , isNativeClassDictionaryId, isNativeOpaqueValueId
                     , isNativeRetainedBinder, isNativeIndexDomainConversion
                     , mkForAllTys, mkInvisFunTys, mkTyVarTys, splitForAllTyCoVars, splitFunTy_maybe
                     , tyVarSpecToBinders)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Coercion (coercionLKind, coercionRKind, coercionRole, isExistsCoRule)
import GHC.Core.DataCon (dataConRepType)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon (TyCon, synTyConRhs_maybe, tyConBinders, tyConInvisTVBinders)
import GHC.Core.TyCon.Set
import GHC.Types.Basic (DefMethSpec(VanillaDM, GenericDM))
import GHC.Types.Id (idCoreRules, idType, isJoinId, isImplicitId, realIdUnfolding)
import GHC.Types.Literal (literalType)
import GHC.Types.Name (Name, isDefaultMethodOcc, isExternalName, nameIsLocalOrFrom, nameModule_maybe, nameOccName)
import qualified GHC.Types.Name.Set as NameSet
import GHC.Types.Tickish (CoreTickish, GenTickish (..))
import GHC.Types.Var
import GHC.Types.Unique.DSet (uniqDSetToList)
import GHC.Types.Var.Set
import GHC.Utils.Panic

import Control.Monad (when)
import Data.Semigroup (Semigroup (..))

-- | Track metadata, variable kinds, and synonyms that the scan has visited.
data ScanState = ScanState
    { scannedMetadata :: !VarSet
    , scannedKinds :: !VarSet
    , scannedSynonyms :: !TyConSet
    , scanMetadata :: !MetadataScan
    , scanPurpose :: !ScanPurpose
    }

-- | Select which retained forms the scan looks for.
data ScanPurpose
    = DetectNative | CheckGentle | CheckUserRules | CheckDepFunUnfolding
    | CheckErasedTypes | DetectScopedDictionaries
    | CheckScopedDictionaryCore | FindScopedSelectorReceiver Id

-- | Select which bound-Id metadata a boundary can demand.
data MetadataScan = AllMetadata | StableMetadata | NoMetadata
    deriving Eq

data ScanRes = ScanRes !Bool !ScanState

-- | Short-circuit a structural scan when it finds a native type.
newtype NativeScan = NativeScan {runNativeScan :: ScanState -> ScanRes}

instance Semigroup NativeScan where
    {-# INLINE (<>) #-}
    NativeScan left <> NativeScan right = NativeScan $ \s ->
        case left s of
            r@(ScanRes True _) -> r
            ScanRes False s'   -> right s'

instance Monoid NativeScan where
    {-# INLINE mempty #-}
    mempty = NativeScan (ScanRes False)

scanList :: (a -> NativeScan) -> [a] -> NativeScan
scanList f = go
  where
    go []       = mempty
    go (x : xs) = NativeScan $ \s ->
        case runNativeScan (f x) s of
            r@(ScanRes True _) -> r
            ScanRes False s'   -> runNativeScan (go xs) s'

{- | Inspect bindings, rules, and metadata on bound identifiers.
Read the source tag of imported unfoldings. Do not follow their templates.
-}
hasNativeDependentProgram :: CoreProgram -> [CoreRule] -> Bool
hasNativeDependentProgram binds rules =
    runScan AllMetadata (scanList scanBind binds <> scanList scanRule rules)

-- | Admit structural indices, fixed call heads and abstract lifted witnesses.
-- Local rules still use the restricted pipeline.
supportsNativeGentle :: CoreProgram -> Bool
supportsNativeGentle binds =
    not (runScanFor CheckGentle AllMetadata (scanList scanBind binds))

-- | Check the bindings and local user rules for the retained optimizer.
supportsNativeRuleBinds :: CoreProgram -> Bool
supportsNativeRuleBinds binds =
    not (runScanFor CheckUserRules AllMetadata (scanList scanBind binds))

-- | Check the expression forms in a retained unfolding without forcing IdInfo.
supportsNativeUserUnfolding :: CoreExpr -> Bool
supportsNativeUserUnfolding expr =
    not (runScanFor CheckUserRules NoMetadata (scanExpr expr))

-- | Check the expression forms in a serialized native DepFunTy unfolding.
supportsNativeDepFunUnfolding :: CoreExpr -> Bool
supportsNativeDepFunUnfolding expr =
    not (runScanFor CheckDepFunUnfolding NoMetadata (scanExpr expr))

-- | Admit the scoped dictionary codec and the retained binders it needs.
-- Other retained proofs and computations still require separate validation.
supportsScopedDictionaryCore :: CoreProgram -> Bool
supportsScopedDictionaryCore binds =
    not (runScanFor CheckScopedDictionaryCore AllMetadata (scanList scanBind binds))

-- | Check whether a function telescope binds a native dependent value or dictionary.
hasNativeDepFunType :: Type -> Bool
hasNativeDepFunType = hasDepFunTelescope True

-- | Does the rule head take a retained argument in its function telescope?
hasRetainedRuleHead :: Id -> Bool
hasRetainedRuleHead = hasDepFunTelescope False . idType

hasDepFunTelescope :: Bool -> Type -> Bool
hasDepFunTelescope include_dict = go
  where
    go ty = case coreFullView ty of
      DepFunTy{} -> True
      DepDictTy{} -> include_dict
      ForAllTy _ _ body -> go body
      FunTy{ft_res = res} -> go res
      _ -> False

-- | Check the local rule with the same retained forms as the optimizer.
-- See Note [Rules at a retained function head].
supportsLocalUserRule :: Id -> CoreRule -> Bool
supportsLocalUserRule fun rule@Rule{ru_fn = name, ru_origin = origin, ru_orphan = NotOrphan{}}
  | maybe (isLocalId fun) (== origin) (nameModule_maybe name)
  = userRuleShape purpose fun rule
  where
    purpose | hasRetainedRuleHead fun = CheckGentle
            | otherwise = CheckErasedTypes
supportsLocalUserRule _ _ = False

{- Note [Rules at a retained function head]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the rule forall xs. keep False xs = xs, the retained binder belongs
to keep. The rule payload may contain only ordinary vector types. Inspect
the head's telescope as well as the payload when writing the interface.
The rule for keep b (choose b) also mentions the runtime index in an
operand. Check both shapes with the retained optimizer's existing type
and coercion checks. The producer and the reader lint the whole rule.
An imported use of keep selects the retained optimizer from its type.

The rule can also live in a third module. For a closed rule such as
keep False p = p, the imported head's Name can be the only reference to
retained syntax. Resolve that Name in the loaded type environment before
choosing the interface tag. Preserve the rule's defining module and its
ordinary orphan classification. A reader with only local retained-rule
support requires the old tag's head to belong to the rule module.
-}

-- | Can the producer and reader process this local source rule?
-- See Note [User rules over erased package types].
supportsErasedUserRule :: Id -> CoreRule -> Bool
supportsErasedUserRule fun rule@Rule{ru_fn = name, ru_origin = origin, ru_orphan = NotOrphan{}}
  | maybe (isLocalId fun) (== origin) (nameModule_maybe name)
  = userRuleShape CheckErasedTypes fun rule
supportsErasedUserRule _ _ = False

-- | Check an imported head and its ordinary rule visibility.
-- See Note [Imported heads of erased package rules].
supportsImportedErasedRule :: Id -> CoreRule -> Bool
supportsImportedErasedRule fun rule =
    not (hasRetainedRuleHead fun) && supportsImportedUserRule fun rule

-- | Validate both the imported head and a local anchor, when present.
supportsImportedUserRule :: Id -> CoreRule -> Bool
supportsImportedUserRule fun rule@Rule{ru_fn = name, ru_origin = origin,
                                         ru_local = False, ru_orphan = orphan, ru_args = args}
  | Just owner <- nameModule_maybe name
  , owner /= origin
  , case orphan of
      IsOrphan -> True
      NotOrphan anchor -> NameSet.nameSetAny (is_anchor anchor) (orphNamesOfExprs args)
  = userRuleShape purpose fun rule
  where
    purpose | hasRetainedRuleHead fun = CheckGentle
            | otherwise = CheckErasedTypes
    is_anchor anchor n = nameIsLocalOrFrom origin n && nameOccName n == anchor
supportsImportedUserRule _ _ = False

-- | Check the typed operands shared by local and imported rule heads.
userRuleShape :: ScanPurpose -> Id -> CoreRule -> Bool
userRuleShape purpose fun rule@Rule{ru_fn = name, ru_auto = False}
  | varName fun == name
  , not (isJoinId fun || isImplicitId fun)
  = not (runScanFor purpose StableMetadata (scanAnnotations fun <> scanRule rule))
userRuleShape _ _ _ = False

{- Note [Imported heads of erased package rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A rule for foldr k z (toList s) has an imported head and a local
anchor, toList. A module with only a rule for open (hide x) has no
anchor when both functions are imported. Preserve the ordinary orphan
classification in both cases. The compiler resolves the head in its
defining module and lints the rule against that Id's type. Check the
stored anchor against the free external names on the left. Simplification
can introduce other local names after the anchor was chosen. The rule's origin
remains the module that declared it, which controls orphan visibility.
True orphan rules keep their existing interface tag. A rule with a local
anchor and an imported head needs a separate tag so an older reader can
reject the interface before checking the rule against a local head.
See Note [Orphans] in GHC.Core.InstEnv.
-}

{- Note [User rules over erased package types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A source rule can call functions whose argument or result is an erased
package. For example, open (hide x) = x has ordinary applications on both
sides. Check the head type, binder types, arguments, RHS and stable metadata.
Pack matches its schema, witness and payload. Unpack compares the package
outside the two binders and matches the body under both binders.
See Note [Matching erased packages] in GHC.Core.Rules.

A rule from the defining module can travel with its head across an interface.
Give this rule its own wire tag. The reader resolves the head in the defining
module, checks the same conditions and lints both sides before using it.
The user supplies the semantic equality, as for an ordinary Haskell RULE.
-}

-- | Check whether Quick Look can traverse a type and its coercions.
-- See Note [Retained types and Quick Look] in GHC.Tc.Utils.Unify.
supportsQuickLookType :: Type -> Bool
supportsQuickLookType = not . runScanFor CheckErasedTypes NoMetadata . scanType

-- | Detect lexical existential expressions before selecting an optimizer.
-- Occurrences cannot introduce new expression forms through Id metadata.
hasExistentialExpr :: CoreExpr -> Bool
hasExistentialExpr Pack{} = True
hasExistentialExpr Unpack{} = True
hasExistentialExpr (App f a) = hasExistentialExpr f || hasExistentialExpr a
hasExistentialExpr (Lam _ body) = hasExistentialExpr body
hasExistentialExpr (Let binding body) =
    any hasExistentialExpr (rhssOfBind binding) || hasExistentialExpr body
hasExistentialExpr (Case scrut _ _ alts) = hasExistentialExpr scrut || any go_alt alts
  where
    go_alt (Alt _ _ body) = hasExistentialExpr body
hasExistentialExpr (Cast e _) = hasExistentialExpr e
hasExistentialExpr (Tick _ e) = hasExistentialExpr e
hasExistentialExpr _ = False

-- | Inspect an expression and metadata on its bound identifiers.
hasNativeDependentExpr :: CoreExpr -> Bool
hasNativeDependentExpr = runScan AllMetadata . scanExpr

{- | Inspect one unfolding RHS without forcing bound-Id metadata.
Each nested unfolding has its own deferred validation. See
Note [Native dependent unfolding templates] in GHC.Core.Unfold.Make.
-}
hasNativeDependentExprRhs :: CoreExpr -> Bool
hasNativeDependentExprRhs = runScan NoMetadata . scanExpr

{- | Inspect an RHS and the metadata that occurrence analysis would traverse.
Do not force unstable nested unfolding templates.
-}
hasNativeDependentUnfolding :: CoreExpr -> Bool
hasNativeDependentUnfolding = runScan StableMetadata . scanExpr

-- | Check that a retained signature belongs to its class's vanilla or generic default.
-- See Note [Compulsory retained defaults] in GHC.Iface.Tidy.
isRetainedVanillaDefaultMethod :: Name -> Type -> Bool
isRetainedVanillaDefaultMethod name ty
  | isExternalName name
  , isDefaultMethodOcc (nameOccName name)
  , Just (vis, _, _, dict_ty, _) <- splitFunTy_maybe (snd (splitForAllTyCoVars ty))
  , isInvisibleFunArg vis
  , Just (cls, _) <- getClassPredTys_maybe dict_ty
  , hasNativeDependentExprRhs (Type ty)
  = or [name == method && ty `eqType` expected_dm_ty cls selector dm_spec
       | (selector, Just (method, dm_spec)) <- classOpItems cls]
  | otherwise = False
  where
    expected_dm_ty _   selector VanillaDM         = idType selector
    expected_dm_ty cls _        (GenericDM dm_ty) =
      let cls_bndrs = tyConBinders (classTyCon cls)
          tv_bndrs  = tyVarSpecToBinders (tyConInvisTVBinders cls_bndrs)
          pred      = mkClassPred cls (mkTyVarTys (binderVars cls_bndrs))
      in mkForAllTys [(unmatchableDataConTy, b) | b <- tv_bndrs] (mkInvisFunTys [pred] dm_ty)

-- | Reject native dependent Core before an unsupported program pass.
rejectNativeDependentProgram :: String -> CoreProgram -> [CoreRule] -> IO ()
rejectNativeDependentProgram context binds rules =
    rejectNative context (hasNativeDependentProgram binds rules)

-- | Reject native dependent Core before an unsupported expression pass.
rejectNativeDependentExpr :: String -> CoreExpr -> IO ()
rejectNativeDependentExpr context expr =
    rejectNative context (hasNativeDependentExpr expr)

-- | Detect private dictionary types before preparation or interface construction.
hasScopedDictionaryProgram :: CoreProgram -> Bool
hasScopedDictionaryProgram binds =
    runScanFor DetectScopedDictionaries AllMetadata (scanList scanBind binds)

-- | Detect a private dictionary form in an expression and its annotations.
hasScopedDictionaryExpr :: CoreExpr -> Bool
hasScopedDictionaryExpr = runScanFor DetectScopedDictionaries AllMetadata . scanExpr

-- | Detect a private dictionary form in an interface-visible type.
hasScopedDictionaryType :: Type -> Bool
hasScopedDictionaryType =
    runScanFor DetectScopedDictionaries NoMetadata . scanType

-- | Does this binder occur as the receiver of a selector in the remaining type?
-- See Note [Arguments of a method-indexed function].
isScopedSelectorReceiver :: Id -> Type -> Bool
isScopedSelectorReceiver binder =
    runScanFor (FindScopedSelectorReceiver binder) NoMetadata . scanType

{- Note [Arguments of a method-indexed function]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a signature such as

  inspect :: Choose a => foreach (x :: a) -> foreach (n :: Natural)
                      -> Mark (choose x) n

the selector needs the local identity of x. Substituting a literal for
n preserves that identity. Check the actual receiver throughout the
remaining type, including the domain of a later binder.
-}

-- | Keep experimental dictionary types out of ordinary CorePrep.
rejectScopedDictionaryProgram :: CoreProgram -> IO ()
rejectScopedDictionaryProgram binds =
    rejectScopedDictionaries (scanList scanBind binds)

-- | Keep experimental dictionary applications out of interactive CorePrep.
rejectScopedDictionaryExpr :: CoreExpr -> IO ()
rejectScopedDictionaryExpr = rejectScopedDictionaries . scanExpr

rejectScopedDictionaries :: NativeScan -> IO ()
rejectScopedDictionaries scan =
    when (runScanFor DetectScopedDictionaries AllMetadata scan) $
      throwGhcExceptionIO (Sorry "CorePrep does not support experimental dependent dictionaries")

-- | Run a scan with empty metadata, kind, and synonym caches.
runScan :: MetadataScan -> NativeScan -> Bool
runScan = runScanFor DetectNative

runScanFor :: ScanPurpose -> MetadataScan -> NativeScan -> Bool
runScanFor purpose metadata scan =
    case runNativeScan scan (ScanState emptyVarSet emptyVarSet emptyTyConSet metadata purpose) of
        ScanRes found _ -> found

-- | A native form qualifies for Gentle only if its children and type qualify.
gentleForm :: NativeScan -> NativeScan
gentleForm scan = NativeScan $ \s ->
    case scanPurpose s of
        CheckGentle    -> runNativeScan scan s
        CheckUserRules -> runNativeScan scan s
        CheckDepFunUnfolding -> runNativeScan scan s
        DetectScopedDictionaries -> runNativeScan scan s
        FindScopedSelectorReceiver{} -> runNativeScan scan s
        _           -> ScanRes True s

-- | Check retained binders and operands in the scoped dictionary codec.
dictionaryOperandForm :: NativeScan -> NativeScan
dictionaryOperandForm children = NativeScan $ \s ->
    case scanPurpose s of
      CheckScopedDictionaryCore -> runNativeScan children s
      _ -> runNativeScan (gentleForm children) s

-- | A selector carries a receiver. A dictionary binder only scopes its body.
scopedDictionaryForm :: Maybe Id -> NativeScan -> NativeScan
scopedDictionaryForm receiver children = NativeScan $ \s ->
    case scanPurpose s of
      CheckScopedDictionaryCore -> runNativeScan children s
      CheckDepFunUnfolding -> runNativeScan children s
      FindScopedSelectorReceiver target
        | receiver /= Just target -> runNativeScan children s
      _ -> ScanRes True s

-- | Exclude shape diagnostics from the scan for actual dictionary forms.
unsupportedShape :: Bool -> NativeScan
unsupportedShape invalid = NativeScan $ \s ->
    case scanPurpose s of
      DetectScopedDictionaries -> ScanRes False s
      FindScopedSelectorReceiver{} -> ScanRes False s
      _ -> ScanRes invalid s

-- | Detect an erased form and check its children at each supported use.
erasedForm :: NativeScan -> NativeScan
erasedForm children = NativeScan $ \s ->
    case scanPurpose s of
        DetectNative -> ScanRes True s
        CheckScopedDictionaryCore -> ScanRes True s
        _ -> runNativeScan children s

-- | Report an unsupported compiler operation when a scan finds a native type.
rejectNative :: String -> Bool -> IO ()
rejectNative context found =
    when found
        $ throwGhcExceptionIO
            ( Sorry
                ( "Native dependent Core cannot enter "
                    ++ context
                    ++ ". This operation does not support retained dependent types or branch proofs."
                )
            )

-- | Inspect expression annotations. Pipeline selection also checks the source
-- tag of imported unfoldings. The tag is available before the template.
scanExpr :: CoreExpr -> NativeScan
scanExpr (Var v) = scanAnnotations v <> NativeScan (\s ->
    ScanRes (case scanPurpose s of
      DetectNative -> scanMetadata s == AllMetadata && isGlobalId v
                      && isRetainedUserUnfolding (realIdUnfolding v)
      _ -> False) s)
scanExpr (Lit lit) = scanType (literalType lit)
scanExpr (App fun arg) = scanExpr fun <> scanExpr arg
scanExpr (Lam b body) = scanBinder b <> scanExpr body
scanExpr (Let bind body) = scanBind bind <> scanExpr body
scanExpr (Case scrut b result alts) =
    scanExpr scrut <> scanBinder b <> scanType result <> scanList scanAlt alts
scanExpr (Pack schema witness payload) =
    erasedForm (scanType schema <> scanType witness <> scanExpr payload)
scanExpr (Unpack witness payload package body) =
    erasedForm (scanExpr package <> scanBinder witness <> scanBinder payload <> scanExpr body)
scanExpr (Cast body co) = scanExpr body <> scanCo co
scanExpr (Tick tick body) = scanTick tick <> scanExpr body
scanExpr (Type ty) = scanType ty
scanExpr (Coercion co) = scanCo co

-- | Inspect binding annotations, metadata, and right-hand sides.
scanBind :: CoreBind -> NativeScan
scanBind (NonRec b rhs) = scanBinder b <> scanExpr rhs
scanBind (Rec pairs) = scanList (\(b, rhs) -> scanBinder b <> scanExpr rhs) pairs

-- | Inspect a case alternative and its binders.
scanAlt :: CoreAlt -> NativeScan
scanAlt alt@(Alt con bs rhs)
    | isNativeIndexAliasAlt alt || isNativeIndexAlt alt = gentleForm children
    | otherwise = children
  where
    children = scanAltCon con <> scanList scanBinder bs <> scanExpr rhs

-- | Inspect the type of a case alternative constructor.
scanAltCon :: AltCon -> NativeScan
scanAltCon DEFAULT = mempty
scanAltCon (LitAlt lit) = scanType (literalType lit)
scanAltCon (DataAlt con) = scanType (dataConRepType con)

-- | Inspect annotations on breakpoint variables.
scanTick :: CoreTickish -> NativeScan
scanTick (Breakpoint _ _ ids) = scanList scanAnnotations ids
scanTick _ = mempty

-- | Inspect a variable's type and multiplicity.
scanAnnotations :: Var -> NativeScan
scanAnnotations v = scanType (varType v) <> maybe mempty scanType (varMultMaybe v)

-- | Inspect bound-identifier metadata once per identifier.
scanBinder :: Var -> NativeScan
scanBinder v =
    scanAnnotations v
        <> NativeScan
            ( \s ->
                let seen = scannedMetadata s
                    metadata = scanMetadata s
                in if metadata == NoMetadata || not (isNonCoVarId v) || v `elemVarSet` seen
                    then ScanRes False s
                    else
                        let !s' = s{scannedMetadata = extendVarSet seen v}
                            rules = idCoreRules v
                        in case scanPurpose s' of
                            CheckGentle | not (null rules) -> ScanRes True s'
                            CheckUserRules | not (all (supportsLocalUserRule v) rules) -> ScanRes True s'
                            _ -> runNativeScan (scanList scanRule rules <> scanUnfolding (realIdUnfolding v)) s'
            )

-- | Inspect the declarative part of a rule.
scanRule :: CoreRule -> NativeScan
scanRule BuiltinRule{} = mempty
scanRule (Rule{ru_bndrs = bs, ru_args = args, ru_rhs = rhs}) =
    scanList scanBinder bs <> scanList scanExpr args <> scanExpr rhs

-- | Inspect an unfolding attached to a bound identifier.
scanUnfolding :: Unfolding -> NativeScan
scanUnfolding NoUnfolding = mempty
scanUnfolding BootUnfolding = mempty
scanUnfolding (OtherCon cons) = scanList scanAltCon cons
scanUnfolding (DFunUnfolding{df_bndrs = bs, df_con = con, df_args = args}) =
    scanList scanBinder bs <> scanType (dataConRepType con) <> scanList scanExpr args
scanUnfolding (CoreUnfolding{uf_src = src, uf_tmpl = body}) = NativeScan $ \s ->
    if scanMetadata s == AllMetadata || isStableSource src
        then runNativeScan (scanExpr body) s
        else ScanRes False s

-- | Inspect a referenced variable's annotations once.
scanVarKind :: Var -> NativeScan
scanVarKind v = NativeScan $ \s ->
    let seen = scannedKinds s
    in if v `elemVarSet` seen
        then ScanRes False s
        else runNativeScan (scanAnnotations v) (s{scannedKinds = extendVarSet seen v})

-- | Inspect each synonym body once. Application arguments are scanned separately.
-- Substitution can add native forms only through those arguments.
-- Inspect the uninstantiated body to avoid duplicating argument scans.
scanSynonym :: TyCon -> NativeScan
scanSynonym tc
    | Just rhs <- synTyConRhs_maybe tc = NativeScan $ \s ->
        let seen = scannedSynonyms s
        in if tc `elemTyConSet` seen
            then ScanRes False s
            else runNativeScan (scanType rhs) (s{scannedSynonyms = extendTyConSet seen tc})
    | otherwise = mempty

-- | Inspect syntax and synonym bodies, including unused synonym arguments.
scanType :: Type -> NativeScan
scanType (DepDictTy dict body) =
    scopedDictionaryForm Nothing (unsupportedShape (not (isNativeClassDictionaryId dict))
                          <> scanAnnotations dict <> scanType body)
scanType (ClassAppTy _ inst (TermVarTy dict) arg@(TermVarTy receiver) kind) =
    scopedDictionaryForm (Just receiver) (unsupportedShape (not (isNativeClassDictionaryId dict))
                          <> scanAnnotations dict <> scanType inst <> scanType arg <> scanType kind)
scanType ClassAppTy{} = NativeScan (ScanRes True)
scanType (TyVarTy tv) = scanVarKind tv
scanType (TermVarTy v) =
    dictionaryOperandForm
      (unsupportedShape (not (isNativeOpaqueValueId v || isNativeNamedOpaqueFunctionId v)) <>
       scanVarKind v)
scanType (TermAppTy f arg) = gentleForm (scanCallHead f <> scanType arg)
-- The type binder itself is safe for Quick Look. Check its kind and payload.
scanType (ExistsTy _ binder body) = erasedForm (scanType (varType binder) <> scanType body)
scanType (DepFunTy _ b body) =
    dictionaryOperandForm (scanRetainedIndex b <> scanType body)
scanType (LitTy _) = mempty
scanType (TyConApp tc args) = scanList scanType args <> scanSynonym tc
scanType (AppTy fun arg) = scanType fun <> scanType arg
scanType (FunTy _ ma mult arg res) =
    scanType ma <> scanType arg <> scanType mult <> scanType res
scanType (ForAllTy ma (Bndr var _) body) =
  scanType ma <> scanType (varType var) <> scanType body
scanType (CastTy ty co) = scanType ty <> scanCo co
scanType (CoercionTy co) = scanCo co

-- | Apply the retained binder checks, including Note [Abstract witness domains]
-- in GHC.Core.Type.
scanRetainedIndex :: Var -> NativeScan
scanRetainedIndex v = NativeScan $ \s ->
    case scanPurpose s of
      FindScopedSelectorReceiver{} -> runNativeScan (scanAnnotations v) s
      _ -> runNativeScan (unsupportedShape (not (isNativeRetainedBinder v))) s

-- | Check the head and its annotation without examining the function body.
scanCallHead :: Id -> NativeScan
scanCallHead f = unsupportedShape (not (isNativeIndexFunctionId f || isNativeDepFunctionId f)) <> scanVarKind f

{- Note [Nominal domains in retained Gentle]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The domain proof of a DepFunCo is nominal even if the complete coercion
is representational. The result type uses the backward nominal conversion.
See Note [Retained dependent function coercions] in GHC.Core.TyCo.Rep.
Do not admit a representational-only domain proof or change its role.
Check each endpoint against the retained binder contract. Check the
original proof and scoped body separately.
-}

-- | Inspect coercion annotations and dependencies.
scanCo :: Coercion -> NativeScan
scanCo (DepFunCo _ _ b domain body) =
    gentleForm (scanRetainedIndex b <> scanDomain domain <> scanCo body)
  where
    scanDomain MRefl = mempty
    scanDomain (MCo delta) =
        unsupportedShape (not (coercionRole delta == Nominal
                            && coercionLKind delta `eqType` varType b
                            && isNativeIndexDomainConversion (varType b) target
                            && isNativeRetainedBinderType target))
          <> scanCo delta
      where
        target = coercionRKind delta
scanCo (TermAppCo f co) = NativeScan $ \s ->
    case scanPurpose s of
      CheckDepFunUnfolding -> ScanRes True s
      _ -> runNativeScan (gentleForm (scanCallHead f <> scanCo co)) s
scanCo (Refl ty) = scanType ty
scanCo (GRefl _ ty mco) = scanType ty <> scanMCo mco
scanCo (TyConAppCo _ _ args) = scanList scanCo args
scanCo (AppCo fun arg) = scanCo fun <> scanCo arg
scanCo (CoVarCo cv) = scanVarKind cv
scanCo (AxiomCo rule args)
    -- The proof binds the witness on both sides. Check its argument before
    -- optimization. See Note [Lifting under an erased existential] in GHC.Core.Coercion.
    | isExistsCoRule rule = gentleForm (scanList scanCo args)
    | otherwise = scanList scanCo args
scanCo (HoleCo hole) = scanVarKind (coHoleCoVar hole)
scanCo (SymCo co) = scanCo co
scanCo (TransCo left right) = scanCo left <> scanCo right
scanCo (SelCo _ co) = scanCo co
scanCo (LRCo _ co) = scanCo co
scanCo (InstCo co arg) = scanCo co <> scanCo arg
scanCo (KindCo co) = scanCo co
scanCo (SubCo co) = scanCo co
scanCo (UnivCo{uco_lty = left, uco_rty = right, uco_deps = deps}) =
    scanType left <> scanType right <> scanDeps deps
scanCo (FunCo{fco_ma = ma, fco_mult = mult, fco_arg = arg, fco_res = res}) =
    scanCo ma <> scanCo mult <> scanCo arg <> scanCo res
scanCo (ForAllCo{fco_tcv = var, fco_ma = ma, fco_kind = kind, fco_body = body}) =
    scanCo ma <> scanMCo kind <> scanType (varType var) <> scanCo body

-- | Inspect an optional coercion.
scanMCo :: MCoercion -> NativeScan
scanMCo MRefl = mempty
scanMCo (MCo co) = scanCo co

-- | Inspect dependencies carried by a universal coercion.
scanDeps :: UnivCoDeps -> NativeScan
scanDeps NoUnivCoDeps = mempty
scanDeps (UnivCoDeps{ucd_cvs = cvs, ucd_holes = holes}) =
    scanList scanVarKind (dVarSetElems cvs)
        <> scanList (scanVarKind . coHoleCoVar) (uniqDSetToList holes)
