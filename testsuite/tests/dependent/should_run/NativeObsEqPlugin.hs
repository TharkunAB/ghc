{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq #-}

module Main (main) where

import Data.Kind ( Type )
import Data.Type.Equality ( (:~:) (Refl), gcastWith, sym )
import Dependent.ObsEq
  ( ObsConfig
  , ObsDomain
  , castEq
  , checkObsEq
  , defaultObsConfig
  , domainVia
  , forVar
  , obsEq
  , setPartialPct
  , setTests
  , totalObsConfig
  , usingEq
  )
import ObsProvider ( Nat (..), natToInt, plus, stepUnfoldAdd, whileAdd )

-- 1. Non-structural local unfold (lazy-equivalent to plus on partial spines),
-- tail-recursive accumulator (total-domain equivalent), Collatz, and McCarthy 91.
localUnfoldAdd :: Nat -> Nat -> Nat
localUnfoldAdd m n = step (Just m) n
  where
    step Nothing acc = acc
    step (Just Zero) acc = step Nothing acc
    step (Just (Succ k)) acc = Succ (step (Just k) acc)

accPlus :: Nat -> Nat -> Nat
accPlus Zero acc = acc
accPlus (Succ m) acc = accPlus m (Succ acc)

isEvenNat :: Nat -> Bool
isEvenNat Zero = True
isEvenNat (Succ Zero) = False
isEvenNat (Succ (Succ k)) = isEvenNat k

halfNat :: Nat -> Nat
halfNat Zero = Zero
halfNat (Succ Zero) = Zero
halfNat (Succ (Succ k)) = Succ (halfNat k)

collatz :: Nat -> Nat
collatz Zero = Succ Zero
collatz (Succ Zero) = Succ Zero
collatz n
  | isEvenNat n = collatz (halfNat n)
  | otherwise = collatz (Succ (triple n))
  where
    triple Zero = Zero
    triple (Succ k) = Succ (Succ (Succ (triple k)))

mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 (mc91 (n + 11))

mc91Spec :: Int -> Int
mc91Spec n
  | n > 100 = n - 10
  | otherwise = 91

-- 2. Guarded domain-restricted functions for forVar & checkObsEq.
twiceHalfNat :: Nat -> Nat
twiceHalfNat n
  | isEvenNat n = plus (halfNat n) (halfNat n)
  | otherwise = Succ n

evenPairNatSum :: Nat -> Nat -> Nat
evenPairNatSum a b = plus (twiceHalfNat a) (twiceHalfNat b)

evenNatDomain :: ObsDomain Nat
evenNatDomain = domainVia (\k -> plus k k) halfNat

cfgTotal120 :: ObsConfig
cfgTotal120 = setTests 120 totalObsConfig

-- Observational proofs (with partial demand probes active by default):
localUnfoldLaw :: foreach (m :: Nat) (n :: Nat) -> localUnfoldAdd m n :~: plus m n
localUnfoldLaw _m _n = obsEq

importedUnfoldLaw :: foreach (m :: Nat) (n :: Nat) -> stepUnfoldAdd m n :~: plus m n
importedUnfoldLaw _m _n = obsEq

mc91Law :: foreach (n :: Int) -> mc91 n :~: mc91Spec n
mc91Law _n = obsEq

-- Total-domain observational proofs via checkObsEq:
accPlusLaw :: foreach (m :: Nat) (n :: Nat) -> accPlus m n :~: plus m n
accPlusLaw _m _n = checkObsEq cfgTotal120

importedWhileLaw :: foreach (m :: Nat) (n :: Nat) -> whileAdd m n :~: plus m n
importedWhileLaw _m _n = checkObsEq (setPartialPct 0 (setTests 120 defaultObsConfig))

collatzLaw :: foreach (n :: Nat) -> collatz n :~: Succ Zero
collatzLaw _n = checkObsEq cfgTotal120

collatzLawInline :: foreach (n :: Nat) -> collatz n :~: Succ Zero
collatzLawInline _n = checkObsEq (setPartialPct 0 (setTests 80 defaultObsConfig))

-- Guarded custom domain proofs (1-var and 2-var):
evenLaw :: foreach (n :: Nat) -> (isEvenNat n ~ True) => twiceHalfNat n :~: n
evenLaw n = checkObsEq (forVar n evenNatDomain (setTests 60 totalObsConfig))

evenPairLaw :: foreach (a :: Nat) (b :: Nat) -> (isEvenNat a ~ True, isEvenNat b ~ True) => evenPairNatSum a b :~: plus a b
evenPairLaw a b = checkObsEq (forVar a evenNatDomain (forVar b evenNatDomain (setTests 60 totalObsConfig)))

-- Runtime indexed transport via castEq, usingEq, and localUnfoldLaw:
data Vec a (n :: Nat) where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

vSum :: Vec Int n -> Int
vSum VNil = 0
vSum (VCons x xs) = x + vSum xs

toUnfoldVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (plus m n) -> Vec Int (localUnfoldAdd m n)
toUnfoldVec _m _n v = castEq obsEq v

fromUnfoldVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (localUnfoldAdd m n) -> Vec Int (plus m n)
fromUnfoldVec m n v = case sym (localUnfoldLaw m n) of
  Refl -> v

roundtripUnfoldVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (plus m n) -> Vec Int (plus m n)
roundtripUnfoldVec m n v = fromUnfoldVec m n (toUnfoldVec m n v)

toAccVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (plus m n) -> Vec Int (accPlus m n)
toAccVec _m _n v = gcastWith (checkObsEq cfgTotal120 :: plus m n :~: accPlus m n) v

fromAccVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (accPlus m n) -> Vec Int (plus m n)
fromAccVec m n v = usingEq (accPlusLaw m n) v

roundtripAccVec :: foreach (m :: Nat) (n :: Nat) -> Vec Int (plus m n) -> Vec Int (plus m n)
roundtripAccVec m n v = fromAccVec m n (toAccVec m n v)

-- 3. Same-module large elimination (`payload :: Schema -> Type` with local `FieldTag`)
-- and retained `schemaOf UserSchema` calls alongside active `Dependent.Plugin.ObsEq` demands.
data FieldTag = FId | FActive | FScore
  deriving (Eq, Show)

data Schema = SEnd | SInt FieldTag Schema | SBool FieldTag Schema
  deriving (Eq, Show)

data SchemaName = UserSchema | PostSchema
  deriving (Eq, Show)

schemaOf :: SchemaName -> Schema
schemaOf UserSchema = SInt FId (SBool FActive (SInt FScore SEnd))
schemaOf PostSchema = SInt FId SEnd

payload :: Schema -> Type
payload SEnd        = type ()
payload (SInt _ r)  = type (Int, payload r)
payload (SBool _ r) = type (Bool, payload r)

encodePayload :: foreach (s :: Schema) -> payload s -> [Int]
encodePayload SEnd        ()      = []
encodePayload (SInt _ r)  (n, xs) = n : encodePayload r xs
encodePayload (SBool _ r) (b, xs) = (if b then 1 else 0) : encodePayload r xs

encodePayloadInvis :: foreach (s :: Schema). payload s -> [Int]
encodePayloadInvis @s xs = encodePayload s xs

checkedSchemaRoundtrip :: [Int]
checkedSchemaRoundtrip =
  case localUnfoldLaw (Succ Zero) Zero of
    Refl ->
      encodePayload (schemaOf UserSchema) (7, (True, (42, ())))
        ++ encodePayloadInvis @(schemaOf UserSchema) (7, (True, (42, ())))

runIntAndEvenChecks :: Int -> IO ()
runIntAndEvenChecks x =
  case (mc91Law x, evenLaw (Succ (Succ Zero)), evenPairLaw (Succ (Succ Zero)) (Succ (Succ (Succ (Succ Zero))))) of
    (Refl, Refl, Refl) -> pure ()

main :: IO ()
main = do
  let baseUnfold :: Vec Int (Succ (Succ (Succ Zero)))
      baseUnfold = VCons 7 (VCons 8 (VCons 9 VNil))
      baseAcc :: Vec Int (Succ (Succ (Succ Zero)))
      baseAcc = VCons 3 (VCons 4 (VCons 5 VNil))
      two = Succ (Succ Zero)
      one = Succ Zero

  if vSum (roundtripUnfoldVec (Succ (Succ Zero)) (Succ Zero) baseUnfold) /= 24
    || vSum (roundtripAccVec (Succ (Succ Zero)) (Succ Zero) baseAcc) /= 12
    || natToInt (plus two one) /= 3
    || checkedSchemaRoundtrip /= [7, 1, 42, 7, 1, 42]
    then error "NativeObsEqPlugin: transport mismatch"
    else pure ()

  runIntAndEvenChecks 42

  case (importedUnfoldLaw two one, importedWhileLaw two one, collatzLaw two, collatzLawInline one) of
    (Refl, Refl, Refl, Refl) -> pure ()
