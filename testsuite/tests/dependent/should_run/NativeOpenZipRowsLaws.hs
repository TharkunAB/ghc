{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ListTuplePuns,
             GADTs, KindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns -Werror=overlapping-patterns #-}
module NativeOpenZipRowsLaws (which, choiceNumber) where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenZipRowsVariants (countEither, countLeft, countRight, zipConcrete)
import PackedVecLibrary (Nat(..))
import ZipLengthSource (zipLength)

emptyLeft :: forall (ys :: [Bool]). zipLength ('[] :: [Char]) ys :~: 'Z
emptyLeft = Refl

emptyRight :: forall (x :: Char) (xs :: [Char]). zipLength (x ': xs) ('[] :: [Bool]) :~: 'Z
emptyRight = Refl

bothCons :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
            zipLength (x ': xs) (y ': ys) :~: 'S (zipLength xs ys)
bothCons = Refl

concrete :: forall (ys :: [Char]). zipConcrete '[] ys :~: 'Z
concrete = Refl

rightStep :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
             countRight (x ': xs) (y ': ys) :~: 'S (countRight (x ': xs) ys)
rightStep = Refl

leftStep :: forall (x :: Char) (xs :: [Char]) (y :: Bool) (ys :: [Bool]).
            countLeft (x ': xs) (y ': ys) :~: 'S (countLeft xs (y ': ys))
leftStep = Refl

changeLeft :: forall (x :: Bool) (xs :: [Bool]) (ys :: [Bool]).
              countEither (x ': xs) ('False ': ys) :~: 'S (countEither xs ('False ': ys))
changeLeft = Refl

changeRight :: forall (x :: Bool) (xs :: [Bool]) (ys :: [Bool]).
               countEither (x ': xs) ('True ': ys) :~: 'S (countEither (x ': xs) ys)
changeRight = Refl

newtype Choice (xs :: [Char]) (ys :: [Bool]) = Choice Int

which :: foreach (xs :: [Char]) -> foreach (ys :: [Bool]) -> Choice xs ys
which [] _ = Choice 0
which (_ : _) [] = Choice 1
which (_ : _) ys = Choice (if null ys then 9 else 2)

choiceNumber :: Choice xs ys -> Int
choiceNumber (Choice n) = let _k = (emptyLeft, emptyRight, bothCons, concrete, rightStep, leftStep, changeLeft, changeRight) in n
