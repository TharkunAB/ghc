{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -Wno-orphans #-}
module NativeOpenPreludeZipWithFoldRule () where

import GHC.Exts (build)
import NativeOpenPreludeZipWithFoldProvider

{-# RULES "probe zip fusion" [~1] forall xs ys. zipProbe xs ys =
    build (\cons nil -> foldZip (pairStep cons) nil xs ys) #-}
{-# RULES "probe inverse" [1] foldZip (pairStep (:)) [] = zipProbe #-}
