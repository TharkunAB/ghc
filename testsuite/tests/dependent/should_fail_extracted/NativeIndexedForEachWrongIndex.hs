{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeApplications #-}
module NativeIndexedForEachWrongIndex where
import GHC.Sigma (Sigma(..))
import NativeIndexedForEachCtorProvider
type BadVar = some (t' :: UTerm 'Zero) | BetaSteps ('UVar 'FZ) t'
bad :: BadVar
bad = Pair @(UTerm 'Zero) (UVar FZ) BRefl
