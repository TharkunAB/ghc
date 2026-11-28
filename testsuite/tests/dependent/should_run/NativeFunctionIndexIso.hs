{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeFunctionIndexIso (runIso, runIsoIdentity) where

import BookIsomorphism
import Control.Monad (unless)
import Data.Type.Equality ((:~:)(Refl), trans)
import GHC.Matchability (Matchability)
import NativeFunctionIndexIsoIdentity

symmetric :: Iso a b -> Iso b a
symmetric (MkIso to from left right) = MkIso from to right left

runIso :: IO ()
runIso = case symmetric sample of
    MkIso to from left right ->
        unless (show (to Clear, from True, case left Set of Refl -> case right False of Refl -> True) == "(False,Set,True)") $
            fail "unexpected symmetric iso"

roundTrip :: foreach (x :: a) -> identity (identity x) :~: x
roundTrip _ = Refl

reflIso :: Iso a a
reflIso = MkIso identity identity roundTrip roundTrip

preludeRoundTrip :: foreach (x :: a) -> id (id x) :~: x
preludeRoundTrip _ = Refl

preludeReflIso :: Iso a a
preludeReflIso = MkIso id id preludeRoundTrip preludeRoundTrip

cong :: forall {a} {b} {m :: Matchability} (f :: a %m -> b) x y.
        x :~: y -> f x :~: f y
cong Refl = Refl

composeLeft :: foreach (f :: a -> b) -> foreach (g :: b -> a)
            -> foreach (h :: b -> c) -> foreach (k :: c -> b)
            -> (foreach (x :: a) -> g (f x) :~: x)
            -> (foreach (y :: b) -> k (h y) :~: y)
            -> foreach (x :: a) -> g (k (h (f x))) :~: x
composeLeft f g _ _ gf kh x = trans (cong @g (kh (f x))) (gf x)

isoTrans :: forall a b c. Iso a b -> Iso b c -> Iso a c
isoTrans (MkIso f g gf fg) (MkIso h k kh hk) =
    MkIso @a @c (h . f) (g . k)
        (composeLeft f g h k gf kh)
        (composeLeft k h g f hk fg)

ignored :: foreach (x :: Bool) -> x :~: x
ignored _ = Refl

withoutForcing :: (Bool -> Bool) -> Bool
withoutForcing f = case ignored (f False) of Refl -> True

runIsoIdentity :: IO ()
runIsoIdentity = do
    observe reflIso
    observe preludeReflIso
    observe (isoTrans sample (case sample of MkIso f g left right -> MkIso g f right left))
    unless (withoutForcing (error "forced callback")) (fail "forced callback")
  where
    observe :: Iso Bool Bool -> IO ()
    observe (MkIso to from left right) =
        case left False of
            Refl -> case right True of
                Refl -> unless ((to False, from True, const True (from (error "forced input"))) == (False, True, True)) $
                    fail "unexpected refl iso"
