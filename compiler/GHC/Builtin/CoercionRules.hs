-- | Built-in rules that can occur in an interface coercion.
module GHC.Builtin.CoercionRules
  ( builtInCoAxiomRules
  , boxedPairReconstructionRule
  , proveBoxedPairProjection, proveScopedSigmaClosure, proveScopedLambdaClosure
  , proveScopedLambdaEta
  , proveScopedCaseTree
  , fromNaturalNumRule, proveFromNaturalNum
  ) where

import GHC.Prelude

import GHC.Builtin.Modules (gHC_INTERNAL_TYPENATS)
import GHC.Builtin.WiredIn.TypeLits (typeNatCoAxiomRules)
import GHC.Builtin.WiredIn.Types (promotedTupleDataCon, tupleTyCon)
import GHC.Core.Coercion (existsCoRule, matchableAppProjectionRule, mkNomReflCo, mkSymCo)
import GHC.Core.Coercion.Axiom
import GHC.Core.DataCon
  ( DataCon, HsImplBang(..), dataConImplBangs, dataConName, dataConOrigArgTys, dataConTyCon
  , dataConUnivTyVars, dataConInstOrigArgTys
  , dataConRepArgBinders, isVanillaDataCon )
import GHC.Core.TyCo.Rep (Coercion(AxiomCo), Scaled(..), Type(TyVarTy, TyConApp), scaledThing)
import GHC.Core.TyCo.Compare
  ( eqType, eqTypeX, eqTypes, eqVarBndrs, eqScopedSigmaClosures, eqScopedLambdaClosures
  , eqInstantiatedScopedClosures )
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.FastString
import GHC.Data.Pair
import GHC.Types.Basic (Boxity(Boxed), LeftOrRight(..))
import GHC.Types.Var (ExistsTyFlag(..))
import GHC.Types.Var.Env (emptyInScopeSet, mkRnEnv2)
import GHC.Types.Var.Set
import GHC.Types.Name (Name, nameModule_maybe, nameOccName, stableNameCmp)
import GHC.Types.Name.Occurrence (mkTcOcc, occNameString, sourceTableTermOcc)
import GHC.Types.Unique (getKey, getUnique)
import GHC.Types.Unique.FM (UniqFM, addToUFM)

import GHC.Utils.Encoding (zEncodeString)
import GHC.Utils.Misc (equalLength, lengthIs, snocView)

import Control.Monad (guard, zipWithM, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (evalStateT, get, modify')
import Data.Char (isDigit)
import Data.List (isPrefixOf, sortBy, stripPrefix)
import Data.Maybe (isJust)
import qualified Data.Set as Set

-- | Resolve the stable names written to the interface by CoreToIface.
builtInCoAxiomRules :: UniqFM FastString CoAxiomRule
builtInCoAxiomRules =
    foldl' (\rules (name, rule) -> addToUFM rules name rule) typeNatCoAxiomRules
      [ (boxedPairReconstructionName, boxedPairReconstructionRule)
      , existsCoRule ExplicitExists Nominal, existsCoRule ExplicitExists Representational
      , existsCoRule ImplicitExists Nominal, existsCoRule ImplicitExists Representational
      , matchableAppProjectionRule CLeft, matchableAppProjectionRule CRight
      , (scopedSigmaClosureName, scopedSigmaClosureRule)
      , (scopedLambdaClosureName, scopedLambdaClosureRule)
      , (scopedLambdaEtaName, scopedLambdaEtaRule)
      , (scopedCaseTreeName, scopedCaseTreeRule)
      , (fromNaturalNumName, fromNaturalNumRule) ]

fromNaturalNumName :: FastString
fromNaturalNumName = fsLit "FromNaturalNum"

-- | Built-in nominal reduction step for FromNatural on a certified Peano type.
fromNaturalNumRule :: CoAxiomRule
fromNaturalNumRule = BuiltInEqRule fromNaturalNumName proves
  where
    proves [Pair lhs _, Pair rhs result]
      | validFromNaturalNumStep lhs rhs = Just (Pair lhs result)
    proves _ = Nothing

-- | Produce nominal evidence for one FromNatural step from a certified Num instance.
proveFromNaturalNum :: Type -> Type -> Maybe Coercion
proveFromNaturalNum lhs rhs
  | validFromNaturalNumStep lhs rhs
  = Just (AxiomCo fromNaturalNumRule [mkNomReflCo lhs, mkNomReflCo rhs])
  | otherwise = Nothing

validFromNaturalNumStep :: Type -> Type -> Bool
validFromNaturalNumStep lhs rhs
  | Just (fam_tc, [target, lit_ty]) <- splitTyConApp_maybe lhs
  , nameOccName (tyConName fam_tc) == mkTcOcc "FromNatural"
  , nameModule_maybe (tyConName fam_tc) == Just gHC_INTERNAL_TYPENATS
  , Just k <- isNumLitTy lit_ty
  , k >= 0
  , Just (tc, []) <- splitTyConApp_maybe target
  , isNativeIndexType target
  , not (isGadtSyntaxTyCon tc)
  , [c1, c2] <- tyConDataCons tc
  , Just (con_tc, con_args) <- splitTyConApp_maybe rhs
  , Just con <- isPromotedDataCon_maybe con_tc
  , let other = if con == c1 then c2 else c1
  , con == c1 || con == c2
  = if k == 0
      then null con_args
           && null (dataConOrigArgTys con)
           && is_unary_peano tc other
      else case con_args of
             [arg_ty] ->
               is_unary_peano tc con
               && null (dataConOrigArgTys other)
               && arg_ty `eqType` mkTyConApp fam_tc [target, mkNumLitTy (k - 1)]
             _ -> False
  | otherwise = False
  where
    is_unary_peano tc succ_con
      | [Scaled _ field] <- dataConOrigArgTys succ_con
      , [HsLazy] <- dataConImplBangs succ_con
      , Just (field_tc, []) <- splitTyConApp_maybe field
      = field_tc == tc
      | otherwise = False

scopedSigmaClosureRule, scopedLambdaClosureRule, scopedLambdaEtaRule, scopedCaseTreeRule :: CoAxiomRule
scopedSigmaClosureRule = scopedClosureRule scopedSigmaClosureName eqScopedSigmaClosures
scopedLambdaClosureRule = scopedClosureRule scopedLambdaClosureName eqScopedLambdaClosures
scopedLambdaEtaRule = scopedClosureRule scopedLambdaEtaName eqScopedLambdaEta
scopedCaseTreeRule = scopedClosureRule scopedCaseTreeName eqScopedCaseTrees

scopedClosureRule :: FastString -> (Type -> Type -> Bool) -> CoAxiomRule
scopedClosureRule name equal = BuiltInEqRule name proves
  where
    -- Compare the two left endpoints. Lifting may change each right endpoint.
    -- The second nominal premise connects the result to the rewritten RHS.
    proves [Pair lhs _, Pair rhs result]
      | equal lhs rhs = Just (Pair lhs result)
    proves _ = Nothing

-- | Produce nominal evidence for equal scoped retained-sum codomains.
proveScopedSigmaClosure :: Type -> Type -> Maybe Coercion
proveScopedSigmaClosure lhs rhs
  | eqScopedSigmaClosures lhs rhs
  = Just (AxiomCo scopedSigmaClosureRule [mkNomReflCo lhs, mkNomReflCo rhs])
  | otherwise = Nothing

-- | Produce nominal evidence for alpha-equivalent generated type lambdas.
proveScopedLambdaClosure :: Type -> Type -> Maybe Coercion
proveScopedLambdaClosure lhs rhs
  | eqScopedLambdaClosures lhs rhs
  = Just (AxiomCo scopedLambdaClosureRule [mkNomReflCo lhs, mkNomReflCo rhs])
  | otherwise = Nothing

-- | Prove equality between a generated lambda and the family it forwards to.
proveScopedLambdaEta :: Type -> Type -> Maybe Coercion
proveScopedLambdaEta lhs rhs
  | eqScopedLambdaEta lhs rhs = Just (evidence lhs rhs)
  | eqScopedLambdaEta rhs lhs = Just (mkSymCo (evidence rhs lhs))
  | otherwise = Nothing
  where
    evidence lambda family = AxiomCo scopedLambdaEtaRule
      [mkNomReflCo lambda, mkNomReflCo family]

{- Note [Eta for a forwarding type lambda]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For 'Map (\a -> Stamp e a) xs', the generated closure has one equation.
After supplying its captures, that equation passes the remaining binder
directly to 'Stamp e'. Compare that prefix with the named family at the
same unmatchable kind. Inspect the raw RHS spine because application
splitting preserves saturation for type families.

Use the branch patterns as the substitution domain. Their order can
differ from the order of the axiom binders. Remove the final binder only
if it is absent from the prefix, including its kinds. A single variable
equation covers every application. A case split or a repeated pattern
would require a separate proof.

The named family's equations are opaque to the forwarding check. For
example, '\a -> Discard e a' forwards to 'Discard e', while '\a -> e' has
no forwarded argument. The equality gives no inverse inference for 'Discard'.
Opaque partial families still have the MaybeApart result.

When the named family is a closed family with a single variable-pattern
equation, reduction can rewrite the body before the closure is synthesized.
In that case, compare the capture-instantiated remaining binders and
right-hand sides of both single-equation families up to alpha-equivalence.
See Note [Unmatchable applications in immutable unification] in GHC.Core.Unify.
-}

-- | Check whether a generated lambda is eta-equivalent to a named family.
eqScopedLambdaEta :: Type -> Type -> Bool
eqScopedLambdaEta lhs rhs
  | Just (lambda, args) <- splitTyConApp_maybe lhs
  , Just (scope, False) <- closedFamilyScope_maybe lambda
  , scope == tyConName lambda
  , isUnsaturatedUserFamily lambda args
  , Just (family, expected) <- splitTyConApp_maybe rhs
  , Nothing <- closedFamilyScope_maybe family
  , isUnsaturatedUserFamily family expected
  , typeKind lhs `eqType` typeKind rhs
  = forwards_family lambda args family expected
    || (isJust (sourceTableTermOcc (nameOccName (tyConName family)))
        && eqInstantiatedScopedClosures (scope, tyConName family) lambda args family expected)
  | otherwise = False
  where
    forwards_family lambda args family expected
      | lengthIs args (tyConArity lambda - 1)
      , Just (FTF_T_T, _, _, _, _) <- splitFunTy_maybe (typeKind lhs)
      , Just ax <- closedFamilyTyConCoAxiom_maybe lambda
      , coAxiomRole ax == Nominal, coAxiomTyCon ax == lambda
      , Just branch <- coAxiomSingleBranch_maybe ax
      , null (cab_cvs branch), null (cab_eta_tvs branch)
      , null (cab_incomps branch), null (cab_overlaps branch)
      , Just vars <- traverse repGetTyVar_maybe (cab_lhs branch)
      , let var_set = mkVarSet vars
      , sizeVarSet var_set == length vars
      , var_set == mkVarSet (cab_tvs branch)
      , all (== Nominal) (cab_roles branch)
      , Just (captures, binder) <- snocView vars
      , equalLength captures args
      , TyConApp head_tc applied <- cab_rhs branch
      , head_tc == family
      , Just (prefix, TyVarTy final) <- snocView applied
      , final == binder
      , not (binder `elemVarSet` tyCoVarsOfTypes prefix)
      = eqTypes (substTysWith captures args prefix) expected
      | otherwise = False

-- | Produce nominal evidence for equal retained constructor case trees.
proveScopedCaseTree :: Type -> Type -> Maybe Coercion
proveScopedCaseTree lhs rhs
  | eqScopedCaseTrees lhs rhs
  = Just (AxiomCo scopedCaseTreeRule [mkNomReflCo lhs, mkNomReflCo rhs])
  | otherwise = Nothing

-- | Recover the owner of a checked retained signature case.
signatureCaseOwner :: TyCon -> Maybe Name
signatureCaseOwner tc = do
  (owner, False) <- closedFamilyScope_maybe tc
  owner_module <- nameModule_maybe owner
  guard (Just owner_module == nameModule_maybe (tyConName tc))
  guard ("$signature-case:" `isPrefixOf` occNameString (nameOccName owner))
  pure owner

caseOcc :: TyCon -> String
caseOcc = occNameString . nameOccName . tyConName

isSignatureCase :: TyCon -> Bool
isSignatureCase tc
  | Just owner <- signatureCaseOwner tc
  = ("$case:" ++ occNameString (nameOccName owner) ++ ":") `isPrefixOf` caseOcc tc
  | otherwise = False

plain_var :: Type -> Maybe TyVar
plain_var (TyVarTy var) = Just var
plain_var _ = Nothing

checkedCaseFields :: [Type] -> Type -> DataCon -> TyCon -> [Type] -> Type -> Maybe [TyVar]
checkedCaseFields parent_args scrutinee_kind con pattern_tc arguments pattern_ty = do
  guard (isVanillaDataCon con && null (dataConRepArgBinders con))
  guard (dataConUnivTyVars con `equalLength` parent_args)
  guard (arguments `lengthIs` tyConArity pattern_tc)
  let (parameters, fields) = splitAt (length parent_args) arguments
  guard (parameters `eqTypes` parent_args && typeKind pattern_ty `eqType` scrutinee_kind)
  vars <- traverse plain_var fields
  let field_kinds = map scaledThing (dataConInstOrigArgTys con parent_args)
  guard (map tyVarKind vars `eqTypes` field_kinds)
  pure vars

-- | The owner and exact generated prefix for the nodes of one case tree.
type CaseTreeScope = (Name, String)

caseTreeScope :: TyCon -> Maybe CaseTreeScope
caseTreeScope tc = do
  owner <- signatureCaseOwner tc
  let prefix = "$case-node:" ++ occNameString (nameOccName owner) ++ ":"
  if isSignatureCase tc
    then pure (owner, prefix ++ zEncodeString (caseOcc tc) ++ ".")
    else do
      node <- stripPrefix prefix (caseOcc tc)
      (root, '.' : number) <- pure (break (== '.') node)
      guard (not (null root) && not (null number) && all isDigit number)
      pure (owner, prefix ++ root ++ ".")

-- | Check two case decision trees at the same saturated applications.
-- See Note [Equality of retained case trees].
eqScopedCaseTrees :: Type -> Type -> Bool
eqScopedCaseTrees lhs rhs
  | Just (tc1, as1) <- splitTyConApp_maybe lhs
  , Just (tc2, as2) <- splitTyConApp_maybe rhs
  , tc1 /= tc2
  , Just left <- caseTreeScope tc1
  , Just right <- caseTreeScope tc2
  , isSignatureCase tc1 == isSignatureCase tc2
  , as1 `lengthIs` tyConArity tc1, as2 `lengthIs` tyConArity tc2
  , as1 `eqTypes` as2
  , Just () <- evalStateT (same_node left right [] tc1 tc2) Set.empty
  = True
  | otherwise = False
  where
    owned_rhs (owner, prefix) ty = do
      (tc, args) <- splitTyConAppNoView_maybe ty
      guard (signatureCaseOwner tc == Just owner)
      number <- stripPrefix prefix (caseOcc tc)
      guard (not (null number) && all isDigit number)
      guard (args `lengthIs` tyConArity tc)
      pure (tc, args)

    same_node left right seen tc1 tc2 = do
      lift $ guard (not (any (\(a, b) -> a == tc1 || b == tc2) seen))
      let key = (getKey (getUnique tc1), getKey (getUnique tc2))
      verified <- get
      if Set.member key verified
        then pure ()
        else do
          lift $ guard (tyConKind tc1 `eqType` tyConKind tc2)
          rows1 <- lift $ checkedCaseRows tc1
          rows2 <- lift $ checkedCaseRows tc2
          lift $ guard (equalLength rows1 rows2)
          let has_same_rhs env a b =
                case (owned_rhs left a, owned_rhs right b) of
                  (Just (child1, args1), Just (child2, args2)) -> do
                    lift $ guard (equalLength args1 args2 && and (zipWith (eqTypeX env) args1 args2))
                    same_node left right ((tc1, tc2) : seen) child1 child2
                  (Nothing, Nothing) -> lift $ guard (eqTypeX env a b)
                  _ -> lift Nothing
              same_row (test1, vars1, a) (test2, vars2, b) = do
                lift $ guard (test1 == test2)
                env <- lift $ eqVarBndrs (mkRnEnv2 emptyInScopeSet) vars1 vars2
                has_same_rhs env a b
          zipWithM_ same_row rows1 rows2
          modify' (Set.insert key)

-- | Recheck that a generated node is an exhaustive constructor test or a leaf.
checkedCaseRows :: TyCon -> Maybe [(Maybe (Int, DataCon), [TyVar], Type)]
checkedCaseRows tc = do
  guard (isLiftedTypeKind (tyConResKind tc))
  NotInjective <- pure (tyConInjectivityInfo tc)
  let kinds = map (tyVarKind . binderVar) (tyConBinders tc)
  guard (all (isEmptyVarSet . tyCoVarsOfType) kinds)
  ax <- closedFamilyTyConCoAxiom_maybe tc
  guard (coAxiomTyCon ax == tc && coAxiomRole ax == Nominal)
  guard (null (co_ax_source_proof_rules ax))
  rows <- traverse (checked_row kinds) (fromBranches (coAxiomBranches ax))
  case rows of
    [(Nothing, _, _)] -> pure rows
    (Just (slot, first), _, _) : _ -> do
      let parent = dataConTyCon first
          tests = [ c | (Just (i, c), _, _) <- rows, i == slot, dataConTyCon c == parent ]
          cmp_con a b = stableNameCmp (dataConName a) (dataConName b)
          cmp_row (Just (_, a), _, _) (Just (_, b), _, _) = cmp_con a b
          cmp_row _ _ = EQ
      constructors <- tyConDataCons_maybe parent
      guard (equalLength rows tests && sortBy cmp_con tests == sortBy cmp_con constructors)
      pure (sortBy cmp_row rows)
    _ -> Nothing
  where
    checked_row kinds branch = do
      guard (null (cab_cvs branch) && null (cab_eta_tvs branch))
      guard (equalLength (cab_roles branch) (cab_tvs branch) && all (== Nominal) (cab_roles branch))
      let pats = cab_lhs branch
      guard (equalLength kinds pats)
      pieces <- zipWithM checked_pattern kinds pats
      let vars = concatMap snd pieces
          tests = [(slot, con) | (slot, (Just con, _)) <- zip [0..] pieces]
          pattern_vars = mkVarSet vars
          bound = mkVarSet (cab_tvs branch)
          result = cab_rhs branch
      guard (vars `equalLength` cab_tvs branch && sizeVarSet pattern_vars == length vars)
      guard (pattern_vars `subVarSet` bound)
      guard (tyCoVarsOfType result `subVarSet` bound)
      case tests of
        [] -> pure (Nothing, vars, result)
        [test] -> pure (Just test, vars, result)
        _ -> Nothing

    checked_pattern kind pat
      | Just var <- plain_var pat = do
          guard (tyVarKind var `eqType` kind)
          pure (Nothing, [var])
      | otherwise = do
          (parent, args) <- splitTyConAppNoView_maybe kind
          let enumeration = null args && isEnumerationTyCon parent
          guard (enumeration
                 || (isVanillaAlgTyCon parent && isBoxedDataTyCon parent
                     && not (isGadtSyntaxTyCon parent)
                     && null (tyConStupidTheta parent)
                     && isLiftedTypeKind (typeKind kind)
                     && args `lengthIs` tyConArity parent))
          (promoted, fields) <- splitTyConAppNoView_maybe pat
          con <- isPromotedDataCon_maybe promoted
          guard (dataConTyCon con == parent)
          vars <- if enumeration
                  then guard (null fields) >> pure []
                  else checkedCaseFields args kind con promoted fields pat
          pure (Just con, vars)

{- Note [Equality of retained case trees]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The case builder emits a closed family for each decision node and records
the signature-case owner on the root and its nodes. Compare the emitted
decisions in order and retain the tested column and child arguments.

Each decision node has either one variable-only leaf row or one constructor
column whose rows test distinct constructors of the same datatype and cover
all constructors. Pair pattern variables by position with eqVarBndrs and
compare branch results with eqTypeX. Follow saturated child calls under the
same owner and generated node prefix, rejecting cycles on either path.
-}

{- Note [Strict boxed pair composition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A certified reconstruction of a boxed pair forces the shell but neither
field. Each output field is one of the two input binders. A certified
projection of the reconstruction therefore observes the same shell and
field as a direct projection of the selected input binder. There is no pair
eta law here. In particular a bottom shell is different from a pair
whose two fields are bottom.

Follow each projected field back through strict constructor cases. Its
path ends at a scrutinee which has no such case. Compare the two paths
from that end and use the outermost common scrutinee with the same field.
For example, fst (duplicate q) and snd (duplicate q) can agree on the
first field of q. They need no equation for q itself. The source solver
records the owners of the cases crossed on either side. All steps check
nominal single-branch axioms, binder identity and exact constructors.
Use the same pure check for Lint and for coercion endpoints. Substitution
can make the operand a constructor; the rule remains valid in that case.

The rule accepts an exchange such as fst (swap q) ~ snd q as a special
case: the paths of both sides end at the same projection of q. A compiler
that reads the same binary format but lacks this rule rejects its key
through tcIfaceAxiomRule.
-}

boxedPairReconstructionName :: FastString
boxedPairReconstructionName = fsLit "BoxedPairReconstruction"

-- | Nominal evidence for two projections that observe the same field.
-- See Note [Strict boxed pair composition].
boxedPairReconstructionRule :: CoAxiomRule
boxedPairReconstructionRule = BuiltInEqRule boxedPairReconstructionName proves
  where
    proves [Pair lhs lhs', Pair rhs rhs'] = do
      guard (lhs `eqType` lhs' && rhs `eqType` rhs')
      (l, r) <- projections lhs rhs
      _ <- commonField l r
      pure (Pair lhs rhs)
    proves _ = Nothing

type PairCaseUse = (TyCon, [Type])

-- | A position in the input pair.
data PairField = FirstField | SecondField
  deriving Eq

-- | The branch bodies used by the strict composition law.
data PairCaseBranch = Project PairField | Reconstruct PairField PairField

selectField :: PairField -> (a, a) -> a
selectField FirstField = fst
selectField SecondField = snd

-- | The current field and scrutinee, its two field kinds, and the equations used.
data Projection = Projection PairField Type (Kind, Kind) [PairCaseUse]

-- | Prove that two projections observe the same field.
proveBoxedPairProjection :: Type -> Type -> Maybe (Coercion, [PairCaseUse])
proveBoxedPairProjection lhs rhs = do
    (l, r) <- projections lhs rhs
    uses <- commonField l r
    pure (AxiomCo boxedPairReconstructionRule [mkNomReflCo lhs, mkNomReflCo rhs], uses)

projections :: Type -> Type -> Maybe (Projection, Projection)
projections lhs rhs = do
    l <- project lhs
    r <- project rhs
    guard (typeKind lhs `eqType` typeKind rhs)
    pure (l, r)
  where
    project ty = do
      (fun, args, Project field, p, a, b) <- appliedPairCase ty
      pure (Projection field p (a,b) [(fun, args)])

-- | Cross one certified reconstruction and retain its input field identities.
throughReconstruction :: Projection -> Maybe Projection
throughReconstruction (Projection field p (a,b) uses) = do
    (fun, args, Reconstruct left right, q, x, y) <- appliedPairCase p
    guard (a `eqType` selectField left (x,y) && b `eqType` selectField right (x,y))
    pure (Projection (selectField field (left,right)) q (x,y) ((fun,args):uses))

matchingField :: Projection -> Projection -> Maybe [PairCaseUse]
matchingField (Projection f p (a,b) uses) (Projection g q (x,y) other) = do
    guard (f == g && a `eqType` x && b `eqType` y && p `eqType` q)
    pure (reverse uses ++ reverse other)

-- | Stop at the outermost common projection. Its scrutinee can remain opaque.
commonField :: Projection -> Projection -> Maybe [PairCaseUse]
commonField l r
  | Just uses <- matchingField l r = Just uses
  | otherwise = commonPrefix (path l []) (path r [])
  where
    path current outer = case throughReconstruction current of
      Just inner -> path inner (current:outer)
      Nothing    -> current:outer

    commonPrefix (x:xs) (y:ys) = do
      uses <- matchingField x y
      case commonPrefix xs ys of
        Just shorter -> Just shorter
        Nothing      -> Just uses
    commonPrefix _ _ = Nothing

-- | Read the single branch at a saturated application and check its kinds.
appliedPairCase :: Type -> Maybe (TyCon, [Type], PairCaseBranch, Type, Kind, Kind)
appliedPairCase ty = do
    (family, args) <- splitTyConApp_maybe ty
    _ <- sourceTableTermOcc (nameOccName (tyConName family))
    guard (length args == tyConArity family)
    p : rev_kinds <- pure (reverse args)
    visible : invis <- pure (reverse (tyConBinders family))
    guard (isVisibleTyConBinder visible && all isInvisibleTyConBinder invis)
    NotInjective <- pure (tyConInjectivityInfo family)
    ax <- closedFamilyTyConCoAxiom_maybe family
    guard (coAxiomRole ax == Nominal && coAxiomTyCon ax == family)
    [branch] <- pure (fromBranches (coAxiomBranches ax))
    guard (null (coAxBranchCoVars branch) && null (coAxBranchIncomps branch))
    let binders = coAxBranchTyVars branch
    y : x : rev_kind_vars <- pure (reverse binders)
    let kind_vars = reverse rev_kind_vars
        kinds = reverse rev_kinds
    guard (length kind_vars == length kinds)
    guard (coAxBranchRoles branch == replicate (length binders) Nominal)
    guard (all (isLiftedTypeKind . varType) kind_vars && all (isLiftedTypeKind . typeKind) kinds)
    pat : rev_pat_kinds <- pure (reverse (coAxBranchLHS branch))
    guard (eqTypes rev_pat_kinds (mkTyVarTys rev_kind_vars))
    (pc, [ka, kb, xt, yt]) <- splitTyConAppNoView_maybe pat
    guard (pc == promotedTupleDataCon Boxed 2)
    guard (x /= y && repGetTyVar_maybe xt == Just x && repGetTyVar_maybe yt == Just y)
    guard (varType x `eqType` ka && varType y `eqType` kb)
    ka_var <- repGetTyVar_maybe ka
    kb_var <- repGetTyVar_maybe kb
    guard (length kind_vars == if ka_var == kb_var then 1 else 2)
    a <- lookup ka_var (zip kind_vars kinds)
    b <- lookup kb_var (zip kind_vars kinds)
    guard (typeKind p `eqType` mkTyConApp (tupleTyCon Boxed 2) [a,b])
    let rhs = coAxBranchRHS branch
        field ty
          | repGetTyVar_maybe ty == Just x = Just FirstField
          | repGetTyVar_maybe ty == Just y = Just SecondField
          | otherwise = Nothing
        input_kinds = (a, b)
    (choice, result) <- case field rhs of
      Just f -> pure (Project f, selectField f input_kinds)
      Nothing -> do
        (rc, [left_kind, right_kind, left, right]) <- splitTyConAppNoView_maybe rhs
        guard (rc == pc)
        left_field <- field left
        right_field <- field right
        let kind_vars_by_field = (ka_var, kb_var)
        guard (repGetTyVar_maybe left_kind == Just (selectField left_field kind_vars_by_field))
        guard (repGetTyVar_maybe right_kind == Just (selectField right_field kind_vars_by_field))
        let result_kind f = selectField f input_kinds
        pure (Reconstruct left_field right_field,
              mkTyConApp (tupleTyCon Boxed 2) [result_kind left_field, result_kind right_field])
    guard (typeKind ty `eqType` result)
    pure (family, args, choice, p, a, b)
