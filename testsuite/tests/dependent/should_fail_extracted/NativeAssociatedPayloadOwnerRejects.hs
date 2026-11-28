{-# LANGUAGE DataKinds, DependentTypes, FlexibleInstances, FunctionalDependencies #-}
{-# LANGUAGE ListTuplePuns, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
module NativeAssociatedPayloadOwnerRejects where

import AssociatedPayloadSum (Allowed, PayloadFormat (Body))
import ComposedPayloadSum (First (FirstBody), Second (SecondBody))

class Other a where
  type OtherPacket a

instance Allowed a => Other [a] where
  type OtherPacket [a] = some (tag :: Bool) | Body [a] tag

class First key => BadBoth key where
  type BadScoped key

instance BadBoth [Int] where
  type BadScoped [Int] = some (tag :: Bool) | (FirstBody [Int] tag, SecondBody [Int] tag)

class ByTag (tag :: Bool) where
  type Tagged tag
instance ByTag False where type Tagged False = Int
instance ByTag True where type Tagged True = Char
type InvalidWitness = some (tag :: Bool) | Maybe (Tagged tag)

type InvalidDomain key = some (tag :: Bool) | Maybe (FirstBody key tag)

class Omitted key other | key -> other where
  type OmittedBody key (tag :: Bool)
instance Omitted Bool Int where
  type OmittedBody Bool False = Int
  type OmittedBody Bool True = Char
type InvalidOmitted = some (tag :: Bool) | Maybe (OmittedBody Bool tag)
