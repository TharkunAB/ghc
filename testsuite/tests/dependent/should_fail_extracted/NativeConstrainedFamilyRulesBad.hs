{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications, TypeFamilies, TypeOperators #-}
module NativeConstrainedFamilyRulesBad where
import Data.Type.Equality
import NativeConstrainedFamilyRulesProvider

bad :: Elem [Int] :~: Int
bad = Refl
