{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, FlexibleContexts, GADTs,
             RankNTypes, TypeFamilies, TypeOperators #-}
module NativeRetainedFamilyGivenFail where

import Data.Type.Coercion (Coercion(..))
import Data.Type.Equality ((:~:)(Refl), gcastWith, trans)
import PackedVecLibrary (Nat(..), Vec(..), replicateNative, sameNat, type (+))

data Size = Zero | Succ Size
type family F (xs :: [Bool]) :: Size
type family G (xs :: [Bool]) :: Size
type family W (n :: Size) :: Size
type family Count (n :: Size) :: Size

callHead :: foreach (f :: Size -> Size) -> foreach (g :: Size -> Size) -> foreach (n :: Size)
         -> Count n :~: f n -> W (Count n) :~: W (g n)
callHead _ _ _ Refl = Refl

callArg :: foreach (f :: Size -> Size) -> foreach (n :: Size) -> foreach (m :: Size)
        -> Count n :~: f n -> W (Count n) :~: W (f m)
callArg _ _ _ Refl = Refl

callRole :: foreach (f :: Size -> Size) -> foreach (n :: Size)
         -> Coercion (Count n) (f n) -> W (Count n) :~: W (f n)
callRole _ _ Coercion = Refl

callCycle :: foreach (f :: Size -> Size) -> foreach (n :: Size)
          -> Count n :~: f (Count n) -> W (Count n) :~: W 'Zero
callCycle _ _ Refl = Refl

outside :: foreach (xs :: [Bool]) -> Maybe (F xs :~: G xs) -> W (F xs) :~: W (G xs)
outside _ proof = case proof of
  Just Refl -> Refl
  Nothing -> Refl

distinct :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
         -> F xs :~: G xs -> W (F ys) :~: W (G ys)
distinct _ _ proof = case proof of Refl -> Refl

roleBad :: foreach (xs :: [Bool]) -> Coercion (F xs) (G xs) -> W (F xs) :~: W (G xs)
roleBad _ proof = case proof of Coercion -> Refl

cycleBad :: foreach (xs :: [Bool]) -> F xs :~: 'Succ (F xs) -> W (F xs) :~: W 'Zero
cycleBad _ proof = case proof of Refl -> Refl

type family Closed (b :: Bool) :: Bool where
  Closed 'False = 'False

closedWrong :: foreach (b :: Bool) -> Closed b :~: b -> 'Just (Closed b) :~: 'Just 'True
closedWrong _ Refl = Refl

wrongContinuation :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
                  -> F xs :~: G xs -> W (F ys) :~: W (G ys)
wrongContinuation _ _ proof = gcastWith proof Refl

eliminate :: (a :~: b) -> ((a ~ b) => r) -> r
eliminate Refl continuation = continuation

compWrong :: foreach (xs :: [Bool])
          -> (F xs :~: W (F xs)) -> (F xs :~: F xs) -> (W (G xs) :~: G xs)
          -> F xs :~: G xs
compWrong _ left proof right =
  trans (trans (id left) (eliminate (id proof) Refl)) (id right)

compAmbiguous :: foreach (xs :: [Bool]) -> F xs :~: G xs -> W (F xs) :~: W (G xs)
compAmbiguous _ proof =
  trans (eliminate (id proof) Refl) (eliminate (id proof) Refl)

consume :: foreach (xs :: [Bool]) -> F xs :~: G xs -> ()
consume _ proof = eliminate (id proof) ()

compOutside :: foreach (xs :: [Bool]) -> F xs :~: G xs -> W (F xs) :~: W (G xs)
compOutside xs proof = case consume xs proof of () -> Refl

firstVec :: Vec a (1 + n) -> a
firstVec (VCons x _) = x

wrongTail :: Num Nat => foreach (extent :: Nat) -> Maybe Char
wrongTail extent = case sameNat extent Z of
  Just proof -> gcastWith proof (Just (firstVec (replicateNative extent 'x')))
  Nothing -> Nothing
