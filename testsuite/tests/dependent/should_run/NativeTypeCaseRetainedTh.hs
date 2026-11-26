{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications, TypeOperators #-}
module Main where

import Control.Monad (unless)
import Data.Binary (decode, encode)
import qualified Data.ByteString.Lazy as Bytes
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHCi.TH.Binary ()
import Language.Haskell.TH.Syntax
import NativeRetainedTH (quoted, render)
import NativeTypeCaseRetainedConTh (Copied(..), Quoted(..), CurriedPacked(..), curriedSample)
import qualified NativeTypeCaseRetainedThChooseAcross as CA
import NativeTypeCaseRetainedThQuote (fromQuote, fromReify)
import NativeTypeCaseRetainedThRaw
  (chooseInline, chooseDirect, renderDirect, observeInline, caseData,
   chooseQuoted, renderQuoted, QuotedPick, chooseNested, SpliceFlag(..),
   nestedCases, chooseNestedRetained, outerCapturedQuote, outerCapturedExact,
   retainedCapturedExact, retainedCapturedQuote)
import qualified NativeTypeCaseRetainedThRenderAcross as RA
import NativeTypeCaseRetainedThSeparate ()

checkWireTypes :: IO ()
checkWireTypes = do
  let v = mkName "b"
      t = VarT v
      old = [ForallT [] [] t, ForallVisT [] t, AppT t t, AppKindT t t, SigT t t,
             t, ConT v, PromotedT v, InfixT t v t, UInfixT t v t,
             PromotedInfixT t v t, PromotedUInfixT t v t, ParensT t,
             TupleT 2, UnboxedTupleT 2, UnboxedSumT 2, ArrowT, MulArrowT, EqualityT,
             ListT, PromotedTupleT 2, PromotedNilT, PromotedConsT, StarT, ConstraintT,
             LitT (NumTyLit 0), WildCardT, ImplicitParamT "x" t, ForallMatchT t t]
      tags = map (Bytes.head . encode) old
  unless (tags == [0 .. 28]) (fail (show tags))
  let ix = IndexVarT v
      new = [ForEachT BndrReq v (ConT (mkName "Bool")) ix, ix,
             ForEachT BndrInvis v (ConT (mkName "Bool")) ix,
             CaseT ix [TypeCaseAlt t (ConT v), TypeCaseAlt WildCardT t],
             LetT v StarT (AppT (VarT v) (VarT v)), LamT [v] t]
  unless (map (Bytes.head . encode) new == [29, 30, 29, 31, 32, 33])
    (fail "Unexpected dependent-type Binary tags")
  unless (map (decode . encode) (old ++ new) == old ++ new)
    (fail "Template Haskell Binary round-trip changed a type")

checkWireCons :: IO ()
checkWireCons = do
  let c = mkName "C"
      b = mkName "b"
      t = ConT c
      bang = Bang NoSourceUnpackedness NoSourceStrictness
      old = [NormalC c [], RecC c [], InfixC (bang, t) c (bang, t),
             ForallC [] [] (NormalC c []), GadtC [c] [] t, RecGadtC [c] [] t,
             ForallMatchC t (ForallC [] [] (NormalC c []))]
      fields = [GadtValueArg bang t t, GadtIndexArg b t, GadtValueArg bang t (IndexVarT b)]
      new = RetainedGadtC [c] fields t
      tags = map (Bytes.head . encode) (old ++ [new])
  unless (tags == [0 .. 7]) (fail ("Unexpected Con tags " ++ show tags))
  unless (map (decode . encode) (old ++ [new]) == old ++ [new])
    (fail "Template Haskell Binary round-trip changed a constructor")

observe :: Bool -> String
observe b = render b (quoted b)

observeDirect :: Bool -> String
observeDirect b = renderDirect b (chooseDirect b ())

observeQuoted :: Bool -> String
observeQuoted b = renderQuoted b (chooseQuoted b)

quotedPick :: QuotedPick ('Just 'True) :~: Proxy 'True
quotedPick = Refl

atRuntime :: Bool -> String
atRuntime b = RA.render b (CA.choose b)

atRuntimeMaybe :: Maybe Bool -> String
atRuntimeMaybe m = RA.renderMaybe m (CA.chooseMaybe m)

copiedCon :: Copied Int -> (Int, Char, Int)
copiedCon (Copied _ True n False c v) = (n, c, v)
copiedCon _ = error "unexpected copied constructor"

quotedCon :: Quoted Int -> (Int, Char, Int)
quotedCon (Quoted _ True n False c v) = (n, c, v)
quotedCon _ = error "unexpected quoted constructor"

curriedCon :: CurriedPacked -> (Bool, String, Int, Char)
curriedCon (CurriedPacked x y select) = (x == y, show y, select True, select False)

main :: IO ()
main = do
    checkWireTypes
    checkWireCons
    caseEffects <- readFile "case-effects"
    nestedEffects <- readFile "nested-effects"
    unless (caseEffects == "after\nrender\ncase\nbefore\nfinalizer\n") (fail caseEffects)
    unless (nestedEffects == "first\nsecond\nfinalizer\n") (fail nestedEffects)
    print (quoted True, quoted False,
           fromQuote (Proxy @Int) True, fromQuote (Proxy @Int) False,
           fromReify (Proxy @Int) True, fromReify (Proxy @Int) False)
    flag <- newIORef True
    first <- observe <$> readIORef flag
    firstInline <- observeInline <$> readIORef flag
    firstDirect <- observeDirect <$> readIORef flag
    firstQuoted <- observeQuoted <$> readIORef flag
    writeIORef flag False
    second <- observe <$> readIORef flag
    secondInline <- observeInline <$> readIORef flag
    secondDirect <- observeDirect <$> readIORef flag
    secondQuoted <- observeQuoted <$> readIORef flag
    print (first, second)
    print (firstInline, secondInline)
    print (firstDirect, secondDirect)
    print (firstQuoted, secondQuoted, quotedPick)
    print (chooseInline True :: Int, chooseInline False :: Char)
    print (chooseDirect True () :: Int, chooseDirect False () :: Char)
    print caseData
    print (chooseNested () SpliceOn :: Proxy 'True, chooseNested () SpliceOff :: Int)
    print (nestedCases ())
    print (chooseNestedRetained (error "outer index forced") True :: Int,
           chooseNestedRetained True False :: Char)
    print (outerCapturedQuote SpliceOn :: Proxy 'True, outerCapturedQuote SpliceOff :: Int)
    print (outerCapturedExact SpliceOn (Proxy @'False) :: Proxy 'True,
           outerCapturedExact SpliceOff (Proxy @'True) :: Int)
    print (retainedCapturedExact True (error "inner index forced") :: Int,
           retainedCapturedExact False True :: Char)
    print (retainedCapturedQuote (Just True) :: Proxy 'True, retainedCapturedQuote Nothing :: Int)

    ref <- newIORef False
    acFirst <- atRuntime <$> readIORef ref
    writeIORef ref True
    acSecond <- atRuntime <$> readIORef ref
    maybeRef <- newIORef Nothing
    absent <- atRuntimeMaybe <$> readIORef maybeRef
    writeIORef maybeRef (Just (error "forced ignored field"))
    present <- atRuntimeMaybe <$> readIORef maybeRef
    unless ((acFirst, acSecond, absent, present) == ("q", "19", "n", "73")) $
        fail "unexpected selected row"

    print (copiedCon (Copied (error "lazy prefix") True 11 False 'x' 27))
    print (quotedCon (Quoted (error "lazy prefix") True 11 False 'x' 27))
    print (case CopiedVacuous (error "unused index") 29 of CopiedVacuous _ n -> n)
    print (curriedCon curriedSample)
