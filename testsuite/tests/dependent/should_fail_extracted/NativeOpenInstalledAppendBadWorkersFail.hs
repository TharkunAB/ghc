{-# LANGUAGE DependentTypes, ListTuplePuns, StandaloneKindSignatures #-}
{-# OPTIONS_GHC -O2 -fomit-interface-pragmas #-}
module NativeOpenInstalledAppendBadWorkersFail where

import qualified NativeOpenInstalledAppendBadWorkers as BW

type BadRight :: forall a. [a] -> [a] -> [a]
type BadRight xs ys = BW.forceRight xs ys

type BadRuled :: forall a. [a] -> [a] -> [a]
type BadRuled xs ys = BW.ruledAppend xs ys
