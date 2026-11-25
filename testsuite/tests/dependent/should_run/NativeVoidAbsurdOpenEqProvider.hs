{-# LANGUAGE DependentTypes #-}
module NativeVoidAbsurdOpenEqProvider
  ( Iso (..)
  , Empty
  , sumZeroRTo
  , sumZeroRFrom
  , sumZeroLTo
  , sumZeroLFrom
  , sumZeroRIso
  , sumZeroLIso
  , absurdRTo
  , emptyLeftTo
  , nestedVoidTo
  , monoVoidTo
  , maybeSumZeroR
  ) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import Data.Void (Void, absurd)
import GHC.Tuple (Tuple2)

data Empty

-- | Isomorphism between two types with round-trip witnesses.
data Iso (a :: Type) (b :: Type) where
  MkIso
    :: foreach (f :: a -> b)
    -> foreach (g :: b -> a)
    -> (foreach (x :: a) -> g (f x) :~: x)
    -> (foreach (y :: b) -> f (g y) :~: y)
    -> Iso a b

sumZeroRTo :: Either a Void -> a
sumZeroRTo (Left x)  = x
sumZeroRTo (Right v) = case v of {}

sumZeroRFrom :: a -> Either a Void
sumZeroRFrom x = Left x

sumZeroRGF :: foreach (s :: Either a Void) -> sumZeroRFrom (sumZeroRTo s) :~: s
sumZeroRGF (Left _)  = Refl
sumZeroRGF (Right v) = case v of {}

sumZeroRFG :: foreach (x :: a) -> sumZeroRTo (sumZeroRFrom x) :~: x
sumZeroRFG _ = Refl

sumZeroRIso :: Iso (Either a Void) a
sumZeroRIso = MkIso sumZeroRTo sumZeroRFrom sumZeroRGF sumZeroRFG

sumZeroLTo :: Either Void a -> a
sumZeroLTo (Left v)  = case v of {}
sumZeroLTo (Right x) = x

sumZeroLFrom :: a -> Either Void a
sumZeroLFrom x = Right x

sumZeroLGF :: foreach (s :: Either Void a) -> sumZeroLFrom (sumZeroLTo s) :~: s
sumZeroLGF (Left v)  = case v of {}
sumZeroLGF (Right _) = Refl

sumZeroLFG :: foreach (x :: a) -> sumZeroLTo (sumZeroLFrom x) :~: x
sumZeroLFG _ = Refl

sumZeroLIso :: Iso (Either Void a) a
sumZeroLIso = MkIso sumZeroLTo sumZeroLFrom sumZeroLGF sumZeroLFG

absurdRTo :: Either a Void -> a
absurdRTo (Left x)  = x
absurdRTo (Right v) = absurd v

emptyLeftTo :: Either Empty a -> a
emptyLeftTo (Left e)  = case e of {}
emptyLeftTo (Right x) = x

nestedVoidTo :: Tuple2 (Either a Void) b -> Tuple2 a b
nestedVoidTo (Left x, y)  = (x, y)
nestedVoidTo (Right v, _) = case v of {}

monoVoidTo :: Either Int Void -> Int
monoVoidTo (Left n)  = n
monoVoidTo (Right v) = case v of {}

maybeSumZeroR :: Maybe (Either a Void) -> Maybe a
maybeSumZeroR Nothing  = Nothing
maybeSumZeroR (Just s) = Just (sumZeroRTo s)
