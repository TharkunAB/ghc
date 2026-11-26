{-# LANGUAGE DependentTypes, ListTuplePuns, NoStandaloneKindSignatures, RoleAnnotations, TypeFamilies, UndecidableInstances #-}
module NativeRetainedPackageProviders
  ( captured, sumWitness, witness, readWitness, atFamily, impossibleCheck
  , Flag(..), Package, Aliased(..), Direct(..)
  , vectorRefl, forwardVector, zeroRefl, matchZero, reverseZero
  , Carrier(..), Linked(..), mkLinked, unsupportedPackageLength
  ) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl), (:~~:)(HRefl))
import GHC.Sigma (Sigma(Pair))
import PackedProjection (Length)
import PackedVecLibrary (Add, Nat(..), PackedVec(..), Vec(..), plus)

type Keep (n :: Nat) (m :: Nat) (x :: Proxy (Add n m)) =
    some (tag :: Bool) | (x :~: x)

captured :: Keep 'Z ('S 'Z) 'Proxy
captured = Pair True Refl

sumWitness :: foreach (m :: Nat) -> foreach (n :: Nat)
           -> some (k :: Nat) | k :~: plus m n
sumWitness m n = Pair (plus m n) Refl

type family Payload (n :: Nat) :: Vec Bool n
witness :: foreach (n :: Nat) -> Proxy (Length ('PackVec n (Payload n)))
witness _ = Proxy

readWitness :: foreach (n :: Nat) -> Proxy n
readWitness n = witness n

atFamily :: foreach (p :: PackedVec a) -> Vec a (Length p)
atFamily (PackVec _ xs) = xs

impossibleCheck :: foreach (xs :: Vec Bool ('S n)) -> Maybe (xs :~~: ('VNil :: Vec Bool 'Z)) -> Bool
impossibleCheck _ proof = case proof of
    Just HRefl -> True
    Nothing -> False

data Flag (b :: Bool) where
  Yes :: Int -> Flag 'True
  No :: Flag 'False

type Package = some (b :: Bool) | Flag b
data Aliased = Aliased { getAlias :: Package }
data Direct = Direct { getDirect :: some (b :: Bool) | Flag b }

vectorRefl :: foreach (xs :: Vec a n) -> xs :~: xs
vectorRefl _ = Refl

forwardVector :: foreach (xs :: Vec a n) -> xs :~: xs
forwardVector xs = vectorRefl xs

zeroRefl :: foreach (xs :: Vec Bool 'Z) -> xs :~: ('VNil :: Vec Bool 'Z)
zeroRefl xs = case xs of
    VNil -> Refl

matchZero :: foreach (xs :: Vec Bool n) -> Maybe (xs :~~: ('VNil :: Vec Bool 'Z))
matchZero xs = case xs of
    VNil -> Just HRefl
    VCons _ _ -> Nothing

reverseZero :: foreach (xs :: Vec a n) -> a :~: Bool ->
    xs :~~: ('VNil :: Vec a 'Z) -> ('VNil :: Vec Bool 'Z) :~~: xs
reverseZero _ Refl HRefl = HRefl

type role Carrier nominal
newtype Carrier (xs :: Vec Bool n) = Carrier Int

type Evidence n p q r =
    some (xs :: Vec Bool n) | (some (ys :: Vec Bool n) | (some (zs :: Vec Bool p) |
    (some (a :: Vec Bool q) | (some (b :: Vec Bool r) |
        (xs :~~: ('VNil :: Vec Bool 'Z), ys :~: xs, zs :~~: ys,
         a :~~: zs, b :~~: a, zs :~~: b, Carrier ys, Carrier zs)))))

data Linked n p q r = Linked (Evidence n p q r)

evidenceZero :: Int -> Evidence 'Z 'Z 'Z 'Z
evidenceZero value = Pair VNil (Pair VNil (Pair VNil (Pair VNil (Pair VNil
    (HRefl, Refl, HRefl, HRefl, HRefl, HRefl, Carrier value, Carrier (value + 2))))))

mkLinked :: Int -> Linked 'Z 'Z 'Z 'Z
mkLinked value = Linked (evidenceZero value)

data Same a where
    Same :: foreach (n :: Nat) -> Vec a 'Z -> Same a
unsupportedPackageLength :: Same a -> Nat
unsupportedPackageLength (Same n _) = n
