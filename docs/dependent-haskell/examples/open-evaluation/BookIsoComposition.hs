{-# LANGUAGE DependentTypes #-}
-- | Symmetry and transitivity from the PLFH Isomorphism chapter.
module BookIsoComposition (isoSym, isoTrans, isoTransWhere) where

import BookIsomorphism (Iso(..))
import Data.Type.Equality ((:~:), trans)
import Data.Type.Equality.Dependent (cong)

-- | Reverse an isomorphism by exchanging the functions and their proofs.
isoSym :: Iso a b -> Iso b a
isoSym (MkIso f g gf fg) = MkIso g f fg gf

-- | Cancel the inner round trip before applying the outer inverse law.
composeLeft :: foreach (f :: a -> b) -> foreach (g :: b -> a)
            -> foreach (h :: b -> c) -> foreach (k :: c -> b)
            -> (foreach (x :: a) -> g (f x) :~: x)
            -> (foreach (y :: b) -> k (h y) :~: y)
            -> foreach (x :: a) -> g (k (h (f x))) :~: x
composeLeft f g _ _ gf kh x = trans (cong g (kh (f x))) (gf x)

-- | Compose two isomorphisms and their round-trip proofs.
isoTrans :: forall a b c. Iso a b -> Iso b c -> Iso a c
isoTrans (MkIso f g gf fg) (MkIso h k kh hk) =
    MkIso @a @c (h . f) (g . k)
        (composeLeft f g h k gf kh)
        (composeLeft k h g f hk fg)

-- | Compose two isomorphisms using local where-clause function aliases.
isoTransWhere :: forall a b c. Iso a b -> Iso b c -> Iso a c
isoTransWhere (MkIso f g gf fg) (MkIso h k kh hk) =
    MkIso @a @c to from
        (composeLeft f g h k gf kh)
        (composeLeft k h g f hk fg)
  where
    to :: a -> c
    to x = h (f x)
    from :: c -> a
    from y = g (k y)
