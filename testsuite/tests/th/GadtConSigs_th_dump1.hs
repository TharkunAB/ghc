{-# LANGUAGE RequiredTypeArguments, TemplateHaskell #-}

module GadtConSigs_th_dump1 where

-- Each quoted forall group records its matchability. The splice preserves
-- the groups in G2 and G3, the required binder in G4, and contexts in G5/G6.
-- See Note [Contexts of nested ForallC] in GHC.ThToHs.
$([d| data G a where
        G1 :: forall a b. (a, b) -> G a
        G2 :: forall a. forall b. (a, b) -> G a
        G3 :: forall a {b}. forall c d. forall e {f} g. G (a, b, c, d, e, f, g)
        G4 :: forall a -> G a
        G5 :: (Eq a, Show a) => a -> G a
        G6 :: Eq a => Show a => a -> G a
    |])
