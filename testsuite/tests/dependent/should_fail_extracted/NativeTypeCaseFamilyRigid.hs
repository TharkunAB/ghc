{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, ScopedTypeVariables, TypeOperators #-}
module NativeTypeCaseFamilyRigid where
import FamilyCasePayload
import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

bad :: forall (input :: Maybe Type). Proxy input -> ChoosePayload Bool input :~: Bool
bad _ = Refl

blocked :: forall (n :: Count). Proxy n -> Down ('S n) :~: 'True
blocked _ = Refl

unknownWidth :: forall (flag :: Bool). Proxy flag -> Width flag 'Nothing :~: 2
unknownWidth _ = Refl
