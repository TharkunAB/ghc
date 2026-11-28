-- | Check the Core of a finite character mapping before publishing its equations.
module GHC.Core.CharCase (
    Certificate,
    certOwner,
    certRows,
    certResultType,
    certDefault,
    charCaseResultType,
    certify,
    Failure (..),
) where

import GHC.Prelude

import Control.Applicative ((<|>))
import Control.Monad (guard, unless, zipWithM)
import GHC.Builtin.WiredIn.Prim (charPrimTy)
import GHC.Builtin.WiredIn.Types
    ( charDataCon, charTy, unboxedUnitDataCon, unboxedUnitTy, unmatchableDataConTy
    )
import GHC.Core (Alt (..), AltCon (..), Bind (..), CoreExpr, Expr (..), collectArgs)
import GHC.Core.DataCon
    ( dataConInstOrigArgTys, dataConTyCon, dataConUnivTyVars, dataConUserTyVars
    , dataConWorkId, isVanillaDataCon, promoteDataCon, promotedDataConMatchabilityArity
    )
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.Multiplicity (scaledThing)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCon
    ( isAbstractTyCon, isAlgTyCon, isBoxedDataTyCon, isLiftedAlgTyCon
    , isTypeDataTyCon, tyConArity, tyConDataCons, tyConStupidTheta
    )
import GHC.Core.Type
    ( FunTyFlag (FTF_T_T), Type, isLiftedTypeKind, isManyTy, isTauTy
    , mkCharLitTy, mkTyConApp, mkVisFunTyManyU, noFreeVarsOfType
    , splitFunTy_maybe, splitTyConApp_maybe, typeKind
    )
import GHC.Core.Utils (exprType)
import GHC.Types.Id (Id, idName, idType, isDataConWorkId_maybe)
import GHC.Types.Literal (Literal (LitChar))
import GHC.Types.Name (Name, isExternalName)
import GHC.Types.Var (isId, varType)
import GHC.Types.Var.Set (elemVarSet)

-- | A strict case with its checked result kind and ordered equations.
data Certificate = Certificate Name Type [(Char, Type)] Default

data Default = InputCharacter | ConstantResult Type

-- | The resolved name of the binding that supplied the case.
certOwner :: Certificate -> Name
certOwner (Certificate owner _ _ _) = owner

-- | The literal alternatives in Core order.
certRows :: Certificate -> [(Char, Type)]
certRows (Certificate _ _ rows _) = rows

-- | The result kind of the generated family.
certResultType :: Certificate -> Type
certResultType (Certificate _ result _ _) = result

-- | Give the default equation its right-hand side.
certDefault :: Certificate -> Type -> Type
certDefault (Certificate _ _ _ InputCharacter) input = input
certDefault (Certificate _ _ _ (ConstantResult result)) _ = result

-- | Recognize unrestricted unary character functions into Char or a closed boxed datatype.
charCaseResultType :: Type -> Maybe Type
charCaseResultType ty
    | Just (FTF_T_T, ma, mult, dom, res) <- splitFunTy_maybe ty
    , ma `eqType` unmatchableDataConTy
    , isManyTy mult
    , dom `eqType` charTy
    , isClosedCharResult res
    = Just res
    | otherwise = Nothing
  where
    isClosedCharResult res
      | res `eqType` charTy = True
      | Just (tc, args) <- splitTyConApp_maybe res
      = isAlgTyCon tc && isBoxedDataTyCon tc && isLiftedAlgTyCon tc
        && not (isTypeDataTyCon tc) && not (isAbstractTyCon tc)
        && null (tyConStupidTheta tc) && length args == tyConArity tc
        && not (null (tyConDataCons tc))
        && all (\dc -> isVanillaDataCon dc && dataConUserTyVars dc == dataConUnivTyVars dc) (tyConDataCons tc)
        && noFreeVarsOfType res && isTauTy res && isLiftedTypeKind (typeKind res)
      | otherwise = False

-- | The first structural requirement the binding failed.
data Failure
    = WrongType
    | UnsupportedBody
    | InvalidContinuation
    | InvalidAlternative
    | AbsentDefault
    | InvalidDefault
    | DuplicateDefault
    | DuplicateLiteral
    | UnsortedLiteral
    deriving (Eq, Show)

{- | Inspect the whole checked body. No test character is supplied.
The caller supplies the desugared binding before optimization.
-}
certify :: Id -> CoreExpr -> Either Failure Certificate
certify owner rhs = do
    let arrow = idType owner
    resultType <- maybe (Left WrongType) Right (charCaseResultType arrow)
    unless (isExternalName (idName owner) && eqType (exprType rhs) arrow) $
        Left WrongType
    (param, body) <- case rhs of
        Lam v e | hasType charTy v -> Right (v, e)
        _ -> Left UnsupportedBody
    (outerInputs, preBox, box) <- peel resultType [param] [] [] body
    (boxed, field, inner) <- case box of
        Case (Var v) bind ty [Alt (DataAlt con) [unboxed] e]
            | v `elem` outerInputs
            , con == charDataCon
            , hasType charTy bind
            , hasType charPrimTy unboxed
            , ty `eqType` resultType ->
                Right (bind, unboxed, e)
        _ -> Left UnsupportedBody
    (lifted, continuations, decision) <- peel resultType (boxed : outerInputs) [field] preBox inner
    (fallback, rows) <- case decision of
        Case (Var v) prim ty alternatives
            | v == field
            , hasType charPrimTy prim
            , ty `eqType` resultType ->
                checkAlternatives resultType lifted [field, prim] continuations alternatives
        _ -> Left UnsupportedBody
    pure (Certificate (idName owner) resultType rows fallback)

{- Note [Character case certificates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The outer case demands the boxed input. The inner case inspects its Char#
field. A character mapping returns a character literal in each alternative
and the original character or a constant character in the default. A finite
classifier returns a ground promoted constructor tree in every branch,
including the default. In all cases the argument must be scrutinized before
choosing a result.

Desugaring can share the default in a local function. Check its body
before accepting a call to it. Its unboxed unit argument must be the unit
constructor itself. An expression of the same type could diverge before
the call. An unused lifted let binding can be dropped. An unlifted binding
can demand its RHS even when its binder is unused.
-}

{- | Follow aliases of the input and validated default continuations.
See Note [Character case certificates].
-}
peel :: Type -> [Id] -> [Id] -> [(Id, Default)] -> CoreExpr
     -> Either Failure ([Id], [(Id, Default)], CoreExpr)
peel result lifted unboxed known (Let (NonRec binder _) rest)
    | isId binder
    , isLiftedTypeKind (typeKind (varType binder))
    , not (binder `elemVarSet` exprFreeVars rest) =
        peel result lifted unboxed known rest
peel result lifted unboxed known (Let (NonRec binder (Var input)) rest)
    | hasType charTy binder
    , input `elem` lifted =
        peel result (binder : lifted) unboxed known rest
peel result lifted unboxed known (Let (NonRec fn (Lam token body)) rest)
    | hasType (mkVisFunTyManyU unboxedUnitTy result) fn
    , hasType unboxedUnitTy token
    , Just fallback <- defaultBody result lifted unboxed known body =
        peel result lifted unboxed ((fn, fallback) : known) rest
    | otherwise = Left InvalidContinuation
peel _ _ _ _ Let{} = Left UnsupportedBody
peel _ lifted _ known expr = Right (lifted, known, expr)

-- | Check the default and collect the ordered literal branches.
checkAlternatives :: Type -> [Id] -> [Id] -> [(Id, Default)] -> [Alt Id]
                  -> Either Failure (Default, [(Char, Type)])
checkAlternatives result lifted unboxed known (Alt DEFAULT [] body : alts) = do
    fallback <- maybe (Left InvalidDefault) Right (defaultBody result lifted unboxed known body)
    rows <- go Nothing alts
    pure (fallback, rows)
  where
    -- See Note [Case expression invariants] in GHC.Core. DEFAULT is first
    -- and the literal keys are strictly ordered.
    go _ [] = Right []
    go _ (Alt DEFAULT _ _ : _) = Left DuplicateDefault
    go previous (Alt (LitAlt (LitChar key)) [] rhs : rest)
        | Just key == previous = Left DuplicateLiteral
        | Just old <- previous, key < old = Left UnsortedLiteral
        | Just value <- literalResult result rhs =
            ((key, value) :) <$> go (Just key) rest
        | otherwise = Left InvalidAlternative
    go _ _ = Left InvalidAlternative
checkAlternatives _ _ _ _ _ = Left AbsentDefault

-- | Reify a closed constructor tree or character literal with no computation.
literalResult :: Type -> CoreExpr -> Maybe Type
literalResult expected (Let (NonRec binder _) rest)
    | isId binder
    , isLiftedTypeKind (typeKind (varType binder))
    , not (binder `elemVarSet` exprFreeVars rest)
    = literalResult expected rest
literalResult expected (App (Var worker) (Lit (LitChar value)))
    | expected `eqType` charTy
    , worker == dataConWorkId charDataCon
    = Just (mkCharLitTy value)
literalResult expected expr
    | (Var worker, raw_args) <- collectArgs expr
    , Just dc <- isDataConWorkId_maybe worker = do
        let tc    = dataConTyCon dc
            univs = dataConUnivTyVars dc
            (ty_args, val_args) = splitAt (length univs) raw_args
        guard (isAlgTyCon tc && isBoxedDataTyCon tc && isLiftedAlgTyCon tc
               && not (isTypeDataTyCon tc) && not (isAbstractTyCon tc)
               && null (tyConStupidTheta tc) && isVanillaDataCon dc
               && dataConUserTyVars dc == univs)
        args <- traverse (\case { Type t -> Just t; _ -> Nothing }) ty_args
        guard (all (\t -> noFreeVarsOfType t && isTauTy t) args
               && mkTyConApp tc args `eqType` expected)
        let field_tys = map scaledThing (dataConInstOrigArgTys dc args)
        guard (length val_args == length field_tys)
        guard (promotedDataConMatchabilityArity dc == 0)
        children <- zipWithM literalResult field_tys val_args
        pure (mkTyConApp (promoteDataCon dc) (args ++ children))
literalResult _ _ = Nothing

-- | Recognize an input character, a ground constructor value, or a checked continuation.
defaultBody :: Type -> [Id] -> [Id] -> [(Id, Default)] -> CoreExpr -> Maybe Default
defaultBody _ _ _ known (App (Var f) (Var a))
    | a == dataConWorkId unboxedUnitDataCon
    , Just fallback <- lookup f known = Just fallback
defaultBody result lifted unboxed _ rhs
    | result `eqType` charTy =
        (case rhs of
           Var v -> InputCharacter <$ guard (v `elem` lifted)
           App (Var f) (Var a) ->
             InputCharacter <$ guard (f == dataConWorkId charDataCon && a `elem` unboxed)
           _ -> Nothing)
        <|> (ConstantResult <$> literalResult result rhs)
    | otherwise = ConstantResult <$> literalResult result rhs

-- | Compare a term binder's type with its expected representation.
hasType :: Type -> Id -> Bool
hasType ty v = isId v && varType v `eqType` ty
