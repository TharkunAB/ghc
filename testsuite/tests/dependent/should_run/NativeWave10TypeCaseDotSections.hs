{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, GADTs #-}
{-# LANGUAGE ListTuplePuns, OverloadedRecordDot, PolyKinds, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, ScopedTypeVariables, TypeAbstractions #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
-- | End-to-end test for runtime Type case, dependent OverloadedRecordDot,
-- T2T operator sections, and R12 superclass selector projection.
module Main (main, LocalRec (..)) where

import Prelude hiding ((+))
import qualified Prelude as P
import Control.Monad (unless)
import Data.Char (toUpper)
import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exists (data Pack)
import GHC.Sigma (Sigma (Pair), first)
import GHC.Sigma.Projection (second)
import GHC.Types (Matchability)
import System.Environment (getArgs)
import Type.Reflection (Typeable)
import NativeWave10TypeCaseDotSectionsProvider

--------------------------------------------------------------------------------
-- 1. Proposal #378 §4.9: Runtime `Type` pattern matching on `foreach (a :: Type)`
--------------------------------------------------------------------------------

defaultVal :: foreach (a :: Type) -> Maybe a
defaultVal a = case a of
  Int    -> Just 0
  Bool   -> Just True
  [Char] -> Just "hi"
  _      -> Nothing

compoundTypeCase :: foreach (b :: Type) (a :: Type) -> b -> Maybe a
compoundTypeCase b a x = case a of
  Maybe Int        -> Just (Just 1)
  Maybe b          -> Just (Just x)
  (Int, Bool)      -> Just (7, False)
  ()               -> Just ()
  type (Int -> Int) -> Just (P.+ 1)
  _                -> Nothing

typeableScopeCase :: forall b. Typeable b => b -> foreach (a :: Type) -> Maybe a
typeableScopeCase x a = case a of
  Maybe b -> Just (Just x)
  Int     -> Just 99
  _       -> Nothing

invisTypeCase :: foreach (a :: Type). Maybe a
invisTypeCase = case a of
  Int  -> Just 42
  Bool -> Just False
  _    -> Nothing

unitOnlyTypeCase :: foreach (a :: Type) -> Maybe a
unitOnlyTypeCase a = case a of
  () -> Just ()
  _  -> Nothing

data PunBox where
  PunBox :: forall a -> a -> (a -> Int) -> PunBox

openPunBoxAndPack :: PunBox -> (exists x | (x, x -> Int)) -> Int
openPunBoxAndPack pb pkg =
  let n1 = case pb of
        PunBox (type a) (v :: a) f -> f v
      n2 = case pkg of
        Pack (type b) pair -> case pair of (w, g) -> g (w :: b)
  in n1 P.+ n2

samplePackPair :: exists x | (x, x -> Int)
samplePackPair = Pack (type Int) (20, (P.+ 7))

checkTypeCase :: Bool
checkTypeCase =
     defaultVal Int == Just 0
  && defaultVal Bool == Just True
  && defaultVal [Char] == Just "hi"
  && defaultVal Double == Nothing
  && compoundTypeCase Char (Maybe Int) 'q' == Just (Just 1)
  && compoundTypeCase Char (Maybe Char) 'q' == Just (Just 'q')
  && compoundTypeCase Char (Int, Bool) 'q' == Just (7, False)
  && compoundTypeCase Char () 'q' == Just ()
  && fmap (\f -> f 4) (compoundTypeCase Char (Int -> Int) 'q') == Just 5
  && compoundTypeCase Char String 'q' == Nothing
  && typeableScopeCase 'z' (Maybe Char) == Just (Just 'z')
  && typeableScopeCase 'z' Int == Just 99
  && typeableScopeCase 'z' Bool == Nothing
  && invisTypeCase @Int == Just 42
  && invisTypeCase @Bool == Just False
  && invisTypeCase @Char == Nothing
  && unitOnlyTypeCase () == Just ()
  && unitOnlyTypeCase Int == Nothing
  && openPunBoxAndPack (PunBox (type Int) 10 (P.+ 5)) samplePackPair == 42

--------------------------------------------------------------------------------
-- 2. Proposal #282 + R04: Dependent `OverloadedRecordDot`
--------------------------------------------------------------------------------

the :: forall (t :: Type) -> t -> t
the _ x = x

data LocalRec a = LocalRec
  { localLen   :: Nat
  , localItems :: Vec a localLen
  }

readCellsTyped :: forall a. foreach (r :: Columns a) -> Vec a (storedLength r)
readCellsTyped r = r.cells

readCellsT2T :: forall a. foreach (r :: Columns a) -> Vec a (storedLength r)
readCellsT2T r = the (Vec a r.storedLength) r.cells

replicateAtDotLen :: forall a. foreach (r :: Columns a) -> a -> Vec a (storedLength r)
replicateAtDotLen r x = vReplicate r.storedLength x

readSigmaSecond :: forall a (b :: a -> Type). foreach (p :: Sigma a b) -> b (first p)
readSigmaSecond p = the (b p.first) p.second

checkDependentDot :: Bool
checkDependentDot =
  let col0 = Columns (S (S Z)) (VCons 'a' (VCons 'b' VNil))
      box0 = ColBox 17 col0
      loc0 = LocalRec (S Z) (VCons (10 :: Int) VNil)
      sig0 :: Sigma Nat (\n -> Vec Char n)
      sig0 = Pair (S (S Z)) (VCons 'x' (VCons 'y' VNil))
      sigLazy :: Sigma Nat (\n -> Vec Int n)
      sigLazy = Pair (S Z) undefined
      projFn :: foreach (r :: Columns Char) -> Vec Char (storedLength r)
      projFn r = (.cells) r
  in natToInt col0.storedLength == 2
     && toList (readCellsTyped col0) == "ab"
     && toList (readCellsT2T col0) == "ab"
     && toList (mapVec toUpper col0.cells) == "AB"
     && toList (replicateAtDotLen col0 'k') == "kk"
     && toList (projFn col0) == "ab"
     && box0.boxTag == 17
     && natToInt box0.innerCols.storedLength == 2
     && (let inner = box0.innerCols in toList inner.cells == "ab")
     && natToInt loc0.localLen == 1
     && toList loc0.localItems == [10]
     && natToInt sig0.first == 2
     && toList (readSigmaSecond sig0) == "xy"
     && toList sig0.second == "xy"
     && natToInt sigLazy.first == 1

--------------------------------------------------------------------------------
-- 3. P3.6: Operator sections in T2T required type arguments
--------------------------------------------------------------------------------

cong :: forall {a} {b} {m :: Matchability}.
        forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
        x :~: y -> f x :~: f y
cong _ Refl = Refl

congRightPlusGround :: forall (m :: Nat) (n :: Nat).
                       m :~: n -> (m + S Z) :~: (n + S Z)
congRightPlusGround eq = cong (+ S Z) eq

congLeftPlusGround :: forall (n1 :: Nat) (n2 :: Nat).
                      n1 :~: n2 -> (S Z + n1) :~: (S Z + n2)
congLeftPlusGround eq = cong (S Z +) eq

congRightPlusRetained :: forall (m :: Nat) (n :: Nat).
                         foreach (p :: Nat) -> m :~: n -> (m + p) :~: (n + p)
congRightPlusRetained p eq = cong (+ p) eq

congLeftPlusRetained :: forall (n1 :: Nat) (n2 :: Nat).
                        foreach (m :: Nat) -> n1 :~: n2 -> (m + n1) :~: (m + n2)
congLeftPlusRetained m eq = cong (m +) eq

congPrefixCons :: forall (a :: Type) (n :: Nat) (x :: a) (xs :: Vec a n) (ys :: Vec a n).
                  xs :~: ys -> VCons x xs :~: VCons x ys
congPrefixCons eq = cong (VCons x) eq

congRightListCons :: forall (a :: Type) (x1 :: a) (x2 :: a) (ys :: [a]).
                     x1 :~: x2 -> (x1 : ys) :~: (x2 : ys)
congRightListCons eq = cong (: ys) eq

congLeftListCons :: forall (a :: Type) (x :: a) (ys1 :: [a]) (ys2 :: [a]).
                    ys1 :~: ys2 -> (x : ys1) :~: (x : ys2)
congLeftListCons eq = cong (x :) eq

checkSections :: Bool
checkSections =
  let r0 = case congRightPlusGround @(S Z) @(S Z) Refl of Refl -> True
      r1 = case congLeftPlusGround @(S Z) @(S Z) Refl of Refl -> True
      r2 = case congRightPlusRetained @(S Z) @(S Z) (S Z) Refl of Refl -> True
      r3 = case congLeftPlusRetained @Z @Z (S Z) Refl of Refl -> True
      r4 = case congPrefixCons @Nat @Z @Z @VNil @VNil Refl of Refl -> True
      r5 = case congRightListCons @Char @'a' @'a' @'[ 'b' ] Refl of Refl -> True
      r6 = case congLeftListCons @Char @'a' @'[] @'[] Refl of Refl -> True
  in r0 && r1 && r2 && r3 && r4 && r5 && r6

--------------------------------------------------------------------------------
-- 4. R12: Superclass projection in scoped class method calls
--------------------------------------------------------------------------------

data FlipBool = FlipBool Bool
  deriving Eq

instance Choose FlipBool where
  choose (FlipBool b) = not b

instance SubChoose FlipBool where
  subLabel (FlipBool b) = if b then 2 else 3

instance SubSubChoose FlipBool where
  subSubLabel (FlipBool b) = if b then 20 else 30

localSubTagged :: forall a. (Eq a, SubChoose a) => foreach (x :: a) -> Tagged (choose x)
localSubTagged x = tagBool (choose x)

observeSub :: SubChoose a => a -> (Bool, Int)
observeSub x = case chooseTagged x of
  Tagged v -> (v, subLabel x)

observeSubForeach :: forall a. SubChoose a => foreach (x :: a) -> Bool
observeSubForeach x = case chooseTagged x of
  Tagged v -> v

observeSubDirect :: forall a. SubChoose a => foreach (x :: a) -> Bool
observeSubDirect x = case tagBool (choose x) of
  Tagged v -> v

observeSubSub :: SubSubChoose a => a -> (Bool, Int)
observeSubSub x = case chooseTagged x of
  Tagged v -> (v, subSubLabel x)

observeSubTaggedPoly :: forall a. SubChoose a => foreach (x :: a) -> (Bool, Int)
observeSubTaggedPoly x = case subTagged x of
  Tagged v -> (v, subLabel x)

observeSubTaggedFromSubSub :: forall a. SubSubChoose a => foreach (x :: a) -> (Bool, Int)
observeSubTaggedFromSubSub x = case subTagged x of
  Tagged v -> (v, subSubLabel x)

observeSubSubTagged :: forall a. SubSubChoose a => foreach (x :: a) -> Bool
observeSubSubTagged x = case subSubTagged x of
  Tagged v -> v

checkR12Superclass :: Bool -> Bool -> Bool
checkR12Superclass t f =
  let ft = FlipBool t
  in observeSub t == (True, 1)
  && observeSub ft == (False, 2)
  && observeSubForeach f == False
  && observeSubForeach (FlipBool f) == True
  && observeSubDirect t == True
  && observeSubDirect ft == False
  && observeSubSub t == (True, 10)
  && observeSubSub (FlipBool f) == (True, 30)
  && observeSubTaggedPoly t == (True, 1)
  && observeSubTaggedPoly ft == (False, 2)
  && observeSubTaggedFromSubSub f == (False, 20)
  && observeSubTaggedFromSubSub (FlipBool f) == (True, 30)
  && observeSubSubTagged t == True
  && observeSubSubTagged ft == False
  && (case subTagged t of Tagged v -> v) == True
  && (case subTagged ft of Tagged v -> v) == False
  && (case localSubTagged f of Tagged v -> v) == False
  && (case localSubTagged ft of Tagged v -> v) == False

main :: IO ()
main = do
  args <- getArgs
  let t = null args
      f = not t
  unless (checkTypeCase && checkDependentDot && checkSections && checkR12Superclass t f) $
    error "NativeWave10TypeCaseDotSections failed"
