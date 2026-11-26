{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns #-}
module NativeFunctionIndexWrongIso where
import BookIsomorphism
import Data.Type.Equality ((:~:)(Refl), trans)
import GHC.Matchability (Matchability)
import NativeFunctionIndexIsoIdentity

wrongField :: Iso a a -> Iso a a
wrongField (MkIso to from left right) = MkIso from to left right

wrongRoundTrip :: foreach (x :: Bool) -> identity (identity x) :~: True
wrongRoundTrip _ = Refl

cong :: forall {a} {b} {m :: Matchability} (f :: a %m -> b) x y. x :~: y -> f x :~: f y
cong Refl = Refl

composeLeft :: foreach (f :: a -> b) -> foreach (g :: b -> a)
            -> foreach (h :: b -> c) -> foreach (k :: c -> b)
            -> (foreach (x :: a) -> g (f x) :~: x)
            -> (foreach (y :: b) -> k (h y) :~: y)
            -> foreach (x :: a) -> g (k (h (f x))) :~: x
composeLeft f g _ _ gf kh x = trans (cong @g (kh (f x))) (gf x)

wrongComposition :: Iso a a -> Iso a a -> Iso a a
wrongComposition (MkIso f g gf _) (MkIso h k kh _) =
    MkIso (h . f) (g . k) (composeLeft f g h k gf kh) (composeLeft f g h k gf kh)
