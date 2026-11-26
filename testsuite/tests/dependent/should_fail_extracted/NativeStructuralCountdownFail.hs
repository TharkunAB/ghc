{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module NativeStructuralCountdownFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeStructuralCountdownLib

wrong :: foreach (n :: Step) -> countdown ('Next n) :~: ('Next n ': countdown n)
wrong _ = Refl
