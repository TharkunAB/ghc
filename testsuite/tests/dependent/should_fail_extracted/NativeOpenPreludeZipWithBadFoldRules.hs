{-# LANGUAGE NoDependentTypes, RankNTypes #-}
{-# OPTIONS_GHC -O2 -Wno-orphans #-}
module NativeOpenPreludeZipWithBadFoldRules () where

import GHC.Exts (build)
import NativeOpenPreludeZipWithBadFold

{-# RULES "change build" [~1]
    forall (g :: forall r. ((Bool, Bool) -> r -> r) -> r -> r).
      build g = [] #-}

{-# RULES "strict zip fusion" [~1] forall xs ys. zipStrictSeed xs ys =
    build (\cons nil -> foldZipStrict (pairStep cons) nil xs ys) #-}
{-# RULES "strict inverse" [1] foldZipStrict (pairStep (:)) [] = zipStrictSeed #-}
