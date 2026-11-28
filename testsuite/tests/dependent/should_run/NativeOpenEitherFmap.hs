{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import DownstreamInstance ()
import GHC.Exts (U)
import NativeOpenEitherFmapBranch (branchProof, literalArrowProof, nestedRightProof, userExample)
import qualified OrdinaryEitherMapping
import PreludeFmapEitherProof (MappedEither, example)
import qualified ResultFunctor as RF
import UniversalResultProof (MappedResult, fmapRight, originalRuntime)

leftProof :: forall a b (f :: a %U -> b) (e :: Bool).
  Proxy f -> Proxy e -> MappedEither f ('Left e :: Either Bool a) :~: ('Left e :: Either Bool b)
leftProof _ _ = Refl

rightProof :: forall a b (f :: a %U -> b) (x :: a).
  Proxy f -> Proxy x -> MappedEither f ('Right x :: Either Bool a) :~: ('Right (f x) :: Either Bool b)
rightProof _ _ = Refl

originalChoice :: forall a (f :: a %U -> Type) (x :: a).
  Proxy f -> Proxy x ->
  MappedResult f ('RF.Succeeded x :: RF.Result Type a) :~: ('RF.Succeeded (f x) :: RF.Result Type Type)
originalChoice = fmapRight @Type

specializedChoice :: forall a (f :: a %U -> Type) (x :: a).
  Proxy f -> Proxy x ->
  RF.translate f ('RF.Succeeded x :: RF.Result Type a) :~: ('RF.Failed (f x) :: RF.Result Type Type)
specializedChoice _ _ = Refl

type family TypeOf (x :: Bool) :: Type where
  TypeOf x = Bool

type family MaybeFlag (x :: Maybe Bool) :: Bool where
  MaybeFlag ('Just b) = b
  MaybeFlag 'Nothing = 'False

main :: IO ()
main = do
  print example
  putStrLn OrdinaryEitherMapping.example
  print userExample
  case ( leftProof @Bool @Type (Proxy @TypeOf) (Proxy @'False)
       , rightProof @Bool @Type (Proxy @TypeOf) (Proxy @'True)
       , branchProof @Bool (Proxy @MaybeFlag)
       , nestedRightProof @Bool (Proxy @MaybeFlag)
       , literalArrowProof
       ) of
    (Refl, Refl, Refl, Refl, Refl) -> pure ()
  let bottom = error "forced an Either field"
      isLeft (Left _) = True
      isLeft (Right _) = False
  if isLeft (fmap bottom (Left bottom :: Either Bool Int)) &&
     not (isLeft (fmap bottom (Right bottom :: Either Bool Int)))
    then pure ()
    else fail "fmap changed the Either constructor"
  spine <- try @ErrorCall (evaluate (fmap id (error "Either spine" :: Either Bool Int)))
  case spine of
    Left _ -> pure ()
    Right _ -> fail "fmap did not inspect the Either spine"
  let input = RF.Succeeded True :: RF.Result Type Bool
      specialized = RF.translate (const (error "specialized callback forced")) input
  print (case originalRuntime input of RF.Succeeded _ -> True; _ -> False)
  print (case specialized of RF.Failed _ -> True; _ -> False)
  print (originalChoice (Proxy @TypeOf) (Proxy @'True), specializedChoice (Proxy @TypeOf) (Proxy @'True))
