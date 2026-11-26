{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             TypeOperators #-}
{-# OPTIONS_GHC -O2 #-}
module NativeRevExplicitListO2WithheldFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeRevExplicitListO2WithheldProvider (revHijack, revInlinable)

badHijack :: forall a. revHijack ('[] :: [a]) :~: '[]
badHijack = Refl

badInlinable :: forall a. revInlinable ('[] :: [a]) :~: '[]
badInlinable = Refl
