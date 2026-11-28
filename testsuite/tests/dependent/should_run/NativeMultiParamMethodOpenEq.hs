{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Main (main) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.Type.Equality ((:~:) (Refl))
import NativeMultiParamMethodOpenEqProvider

-- 1. Multi-parameter LabelledMap (polymorphic Either e, concrete Row Int Bool, pair, Maybe).
lmapEitherComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (s :: Either e a)
  -> lmap f (lmap g s) :~: lmap (f . g) s
lmapEitherComp _ _ (Left _)  = Refl
lmapEitherComp _ _ (Right _) = Refl

lmapRowComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (r :: Row Int Bool a)
  -> lmap f (lmap g r) :~: lmap (f . g) r
lmapRowComp _ _ EmptyRow         = Refl
lmapRowComp _ _ (FlaggedRow _ _) = Refl
lmapRowComp _ _ (ValueRow _ _)   = Refl

lmapPairComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (p :: (e, a))
  -> lmap f (lmap g p) :~: lmap (f . g) p
lmapPairComp _ _ (_, _) = Refl

lmapMaybeFlip
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (m :: Maybe a)
  -> lmap f (lmap g m) :~: lmap (f . g) m
lmapMaybeFlip _ _ Nothing  = Refl
lmapMaybeFlip _ _ (Just _) = Refl

-- 2. Multi-parameter IndexedMap (f i a -> f i b).
imapEitherComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (s :: Either e a)
  -> imap f (imap g s) :~: imap (f . g) s
imapEitherComp _ _ (Left _)  = Refl
imapEitherComp _ _ (Right _) = Refl

imapRowComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (r :: Row Int Bool a)
  -> imap f (imap g r) :~: imap (f . g) r
imapRowComp _ _ EmptyRow         = Refl
imapRowComp _ _ (FlaggedRow _ _) = Refl
imapRowComp _ _ (ValueRow _ _)   = Refl

imapPairComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (p :: (e, a))
  -> imap f (imap g p) :~: imap (f . g) p
imapPairComp _ _ (_, _) = Refl

-- 3. Superclass-constrained mapping class SuperLabelledMap.
slmapEitherComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (s :: Either e a)
  -> slmap f (slmap g s) :~: slmap (f . g) s
slmapEitherComp _ _ (Left _)  = Refl
slmapEitherComp _ _ (Right _) = Refl

slmapRowComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (r :: Row Int Bool a)
  -> slmap f (slmap g r) :~: slmap (f . g) r
slmapRowComp _ _ EmptyRow         = Refl
slmapRowComp _ _ (FlaggedRow _ _) = Refl
slmapRowComp _ _ (ValueRow _ _)   = Refl

-- 4. NewTyCon single-method mapping classes (SoloMap and SoloIndexedMap).
soloEitherComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (s :: Either e a)
  -> soloMap f (soloMap g s) :~: soloMap (f . g) s
soloEitherComp _ _ (Left _)  = Refl
soloEitherComp _ _ (Right _) = Refl

soloRowComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (r :: Row Int Bool a)
  -> soloMap f (soloMap g r) :~: soloMap (f . g) r
soloRowComp _ _ EmptyRow         = Refl
soloRowComp _ _ (FlaggedRow _ _) = Refl
soloRowComp _ _ (ValueRow _ _)   = Refl

soloImapEitherComp
  :: foreach (f :: b -> c)
  -> foreach (g :: a -> b)
  -> foreach (s :: Either e a)
  -> soloImap f (soloImap g s) :~: soloImap (f . g) s
soloImapEitherComp _ _ (Left _)  = Refl
soloImapEitherComp _ _ (Right _) = Refl

-- 5. Certified ground binary methods with superclasses and multi-parameter classes.
eqZeroSucc :: foreach (n :: MyNat) -> (n == MSucc n) :~: False
eqZeroSucc MZero     = Refl
eqZeroSucc (MSucc m) = eqZeroSucc m

eqRefl :: foreach (n :: MyNat) -> (n == n) :~: True
eqRefl MZero     = Refl
eqRefl (MSucc m) = eqRefl m

cmpZeroSucc :: foreach (n :: MyNat) -> compare n (MSucc n) :~: LT
cmpZeroSucc MZero     = Refl
cmpZeroSucc (MSucc m) = cmpZeroSucc m

cmpRefl :: foreach (n :: MyNat) -> compare n n :~: EQ
cmpRefl MZero     = Refl
cmpRefl (MSucc m) = cmpRefl m

semiRightZero :: foreach (n :: MyNat) -> (n <> MZero) :~: n
semiRightZero MZero     = Refl
semiRightZero (MSucc m) = case semiRightZero m of
  Refl -> Refl

tappendRightZero :: foreach (n :: MyNat) -> tappend n MZero :~: n
tappendRightZero MZero     = Refl
tappendRightZero (MSucc m) = case tappendRightZero m of
  Refl -> Refl

mcombineRightZero :: foreach (n :: MyNat) -> mcombine n MZero :~: n
mcombineRightZero MZero     = Refl
mcombineRightZero (MSucc m) = case mcombineRightZero m of
  Refl -> Refl

require :: Bool -> IO ()
require True  = pure ()
require False = throwIO (ErrorCall "assertion failed")

{-# NOINLINE runProofs #-}
runProofs :: (Int -> Int) -> (Int -> Int) -> (Int -> Int) -> IO ()
runProofs inc dbl tri = do
  let two = MSucc (MSucc MZero)
      n0, n1, n2, n3, n4, n5, n7, n9 :: Int
      n0 = 0
      n1 = 1
      n2 = 2
      n3 = 3
      n4 = 4
      n5 = 5
      n7 = 7
      n9 = 9
  case ( lmapEitherComp inc dbl (Right n3 :: Either Bool Int)
       , lmapRowComp inc dbl (FlaggedRow n7 True :: Row Int Bool Int)
       , lmapPairComp inc dbl ('a', n3)
       , lmapMaybeFlip inc dbl (Just n4)
       , imapEitherComp inc dbl (Left 'x' :: Either Char Int)
       , imapRowComp inc tri (ValueRow n9 n2)
       , imapPairComp inc dbl (False, n5)
       , slmapEitherComp inc dbl (Right n2 :: Either Int Int)
       , slmapRowComp inc dbl (EmptyRow :: Row Int Bool Int)
       , soloEitherComp inc dbl (Right n1 :: Either Bool Int)
       , soloRowComp inc dbl (ValueRow n1 n0 :: Row Int Bool Int)
       , soloImapEitherComp inc dbl (Left n0 :: Either Int Int)
       , eqZeroSucc two
       , eqRefl two
       , cmpZeroSucc two
       , cmpRefl two
       , semiRightZero two
       , tappendRightZero two
       , mcombineRightZero two
       ) of
    ( Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl
      , Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl ) -> pure ()

main :: IO ()
main = do
  let two = MSucc (MSucc MZero)
  require (lmap (+ 1) (ValueRow 5 10 :: Row Int Bool Int) == ValueRow 5 11)
  require (imap not (Right True :: Either String Bool) == Right False)
  require (slmap (+ 2) (ValueRow 3 4 :: Row Int Bool Int) == ValueRow 3 6)
  require (soloMap swapEither (Right (Left True) :: Either Int (Either Bool Char)) == Right (Right True))
  require (two == two)
  require (compare MZero two == LT)
  require (tappend two (MSucc MZero) == MSucc two)
  require (mcombine two MZero == two)
  runProofs (+ 1) (* 2) (* 3)
