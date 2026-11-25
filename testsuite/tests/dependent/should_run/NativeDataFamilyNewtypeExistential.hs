{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeDataFamilyNewtypeExistential
  ( Wrap(..)
  , Box(..)
  , Unwrap
  , BoxKind
  , BoxPayload
  , unwrapProof
  , boxKindProof
  , boxPayloadProof
  , keepWrap
  , retainedWrapProof
  ) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))

data family Wrap a
newtype instance Wrap [a] = MkWrap a

data family Box k
data instance Box () = forall a. MkBox a

type Unwrap :: Wrap [a] -> a
type family Unwrap w where
  Unwrap ('MkWrap x) = x

type BoxKind :: Box () -> Type
type family BoxKind b where
  BoxKind ('MkBox @a _) = a

type BoxPayload :: forall (b :: Box ()) -> BoxKind b
type family BoxPayload b where
  BoxPayload ('MkBox @a x) = x

unwrapProof :: Unwrap ('MkWrap 'True) :~: 'True
unwrapProof = Refl

boxKindProof :: BoxKind ('MkBox @Bool 'True) :~: Bool
boxKindProof = Refl

boxPayloadProof :: BoxPayload ('MkBox @Bool 'True) :~: 'True
boxPayloadProof = Refl

keepWrap :: foreach (w :: Wrap [Bool]) -> w :~: w
keepWrap _ = Refl

retainedWrapProof :: 'MkWrap 'True :~: 'MkWrap 'True
retainedWrapProof = keepWrap (MkWrap True)
