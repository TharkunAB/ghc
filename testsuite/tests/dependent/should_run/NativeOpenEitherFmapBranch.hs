{-# LANGUAGE DataKinds, DependentTypes, FlexibleInstances, ListTuplePuns, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeApplications, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -Wno-orphans #-}
module NativeOpenEitherFmapBranch
  ( branchProof
  , nestedRightProof
  , literalArrowProof
  , userExample
  ) where

import CheckedEitherInput (Input)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import NativeOpenEitherFmapProducer (Result(..), Translate(translate, count))
import PreludeFmapEitherProof (MappedEither)

instance Translate Maybe where
  translate = fmap
  count _ = 0

type SelectedInput = Input 'True
type LiteralInput = Input 'False

branchProof :: forall b (f :: Maybe Bool %U -> b). Proxy f ->
  MappedEither f SelectedInput :~: ('Right (f ('Just 'False)) :: Either Bool b)
branchProof _ = Refl

type family IsRight (value :: Either Bool b) :: Bool where
  IsRight ('Right _) = 'True
  IsRight ('Left _) = 'False

nestedRightProof :: forall b (f :: Maybe Bool %U -> b). Proxy f ->
  IsRight (MappedEither f ('Right ('Just 'False))) :~: 'True
nestedRightProof _ = Refl

type family InspectArrow f where
  InspectArrow (Proxy ('Right ('Just 'False) :: Either Bool (Maybe Bool)) -> ()) = 'True

literalArrowProof :: InspectArrow (Proxy LiteralInput -> ()) :~: 'True
literalArrowProof = Refl

type MappedUserResult :: forall e a b. (a %U -> b) -> Result e a -> Result e b
type MappedUserResult f x = translate f x

userLeft :: forall e a b (f :: a %U -> b) (x :: e).
  Proxy f -> Proxy x -> MappedUserResult f ('Failed x :: Result e a) :~: ('Failed x :: Result e b)
userLeft _ _ = Refl

userRight :: forall e a b (f :: a %U -> b) (x :: a).
  Proxy f -> Proxy x -> MappedUserResult f ('Succeeded x :: Result e a) :~: ('Succeeded (f x) :: Result e b)
userRight _ _ = Refl

type family Flip (x :: Bool) :: Bool where
  Flip 'False = 'True
  Flip 'True = 'False

userExample :: (('Failed 'x' :: Result Char Bool) :~: 'Failed 'x',
                ('Succeeded 'False :: Result Bool Bool) :~: 'Succeeded 'False)
userExample = (userLeft @Char (Proxy @Flip) (Proxy @'x'),
               userRight @Bool (Proxy @Flip) (Proxy @'True))
