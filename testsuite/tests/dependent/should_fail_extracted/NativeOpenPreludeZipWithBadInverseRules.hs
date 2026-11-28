{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -Wno-orphans #-}
module NativeOpenPreludeZipWithBadInverseRules () where

import GHC.Exts (build)
import NativeOpenPreludeZipWithFoldProvider (foldZip, pairStep, zipProbe)

{-# RULES "probe zip fusion 2" [~1] forall xs ys. zipProbe xs ys =
    build (\cons nil -> foldZip (pairStep cons) nil xs ys) #-}
{-# RULES "bad inverse" [1] foldZip (pairStep (:)) [] = \_ _ -> [] #-}
