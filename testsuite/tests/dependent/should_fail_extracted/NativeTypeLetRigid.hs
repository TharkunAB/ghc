{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
module NativeTypeLetRigid where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLetPairBindings (Rebuild)
import NativeTypeLetSigned (DependentCase)
import PackedVecLibrary (Nat(..), Vec)
import TypeLet (CaseResult, DimensionWithOverride, Frame)

unknown :: Proxy width -> DimensionWithOverride width 'Nothing :~: 2
unknown _ = Refl

mismatch :: DependentCase (Maybe Bool) ('Just 'True) 'True 'True :~: (Proxy ('Just 'False), Maybe Bool)
mismatch = Refl

rigidCase :: Proxy b -> CaseResult b :~: Either Bool Bool
rigidCase _ = Refl

pairEta :: Proxy dimensions -> Rebuild dimensions :~: dimensions
pairEta _ = Refl

wrongWidth :: Frame 'Z 'True Char :~: Vec Char 'Z
wrongWidth = Refl
