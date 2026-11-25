{-# LANGUAGE DependentTypes, FunctionalDependencies, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module NativeStandaloneConstrainedFamily
  ( Base
  , Collects
  , Allowed
  , Owner
  , ElemTop
  , RestTop
  , DelegateTop
  , ForwardTop
  , MemberTop
  , elemBoolProof
  , delegateBoolProof
  , forwardBoolProof
  , memberBoolProof
  , polyElem
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))

class Base c
class Base c => Collects c
class Allowed a

instance Allowed Bool
instance Base [a]
instance Allowed a => Collects [a]

class Owner c e | c -> e
instance Owner [Bool] Bool

type ElemTop :: Collects c => c -> Type
type family ElemTop c
type instance ElemTop [a] = a

type RestTop :: forall c -> Base c => Type
type family RestTop c
type instance RestTop [a] = [a]

type DelegateTop :: Collects c => c -> Type
type family DelegateTop c where
  DelegateTop c = RestTop c

type ForwardTop :: Collects c => c -> Type
type family ForwardTop c
type instance ForwardTop c = ElemTop c

type MemberTop :: Owner c e => c -> Type
type family MemberTop c
type instance MemberTop [a] = a

elemBoolProof :: ElemTop [Bool] :~: Bool
elemBoolProof = Refl

delegateBoolProof :: DelegateTop [Bool] :~: [Bool]
delegateBoolProof = Refl

forwardBoolProof :: ForwardTop [Bool] :~: Bool
forwardBoolProof = Refl

memberBoolProof :: MemberTop [Bool] :~: Bool
memberBoolProof = Refl

polyElem :: Collects c => Proxy c -> ElemTop c -> DelegateTop c -> ()
polyElem _ _ _ = ()
