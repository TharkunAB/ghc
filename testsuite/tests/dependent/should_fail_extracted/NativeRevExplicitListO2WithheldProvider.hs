{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, RankNTypes #-}
{-# OPTIONS_GHC -O2 -fomit-interface-pragmas -fno-write-if-simplified-core #-}
module NativeRevExplicitListO2WithheldProvider (revHijack, revInlinable) where

import GHC.Exts (build)

{-# RULES "hijackBuildLocal" forall (g :: forall b. (a -> b -> b) -> b -> b). build g = [] #-}

app :: [a] -> [a] -> [a]
app []       ys = ys
app (x : xs) ys = x : app xs ys

revHijack :: [a] -> [a]
revHijack []       = []
revHijack (x : xs) = app (revHijack xs) [x]

{-# INLINABLE revInlinable #-}
revInlinable :: [a] -> [a]
revInlinable []       = []
revInlinable (x : xs) = app (revInlinable xs) [x]
