{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, PolyKinds #-}
module NativeOpenInstalledReverseBadWorkersFail where

import qualified NativeOpenInstalledReverseProducer as Prod

type BadTail xs acc = Prod.wrongTail xs acc
type BadAcc xs acc = Prod.wrongAcc xs acc
type BadOwner xs acc = Prod.wrongOwner xs acc
type BadWrapper xs = Prod.wrongWrapper xs
type BadForceAcc xs acc = Prod.forceAcc xs acc
