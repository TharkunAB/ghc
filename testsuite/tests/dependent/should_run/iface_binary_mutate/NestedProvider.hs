{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module NestedProvider (Tag(..), ChooseStep, OuterMethod) where
import Data.Kind (Type)
import PackedVecLibrary (Nat(..))
import StepSource (stepNat)

data Tag (n :: Nat) = MkTag

type family ChooseStep (n :: Nat) :: Type where
    ChooseStep 'Z = Tag (stepNat 'Z)
    ChooseStep ('S _) = ()

type family InnerMethod :: Maybe Bool where
    InnerMethod = Prelude.fmap Prelude.not ('Just 'True)

type family OuterMethod (n :: Nat) :: Maybe Bool where
    OuterMethod 'Z = InnerMethod
    OuterMethod ('S _) = 'Nothing
