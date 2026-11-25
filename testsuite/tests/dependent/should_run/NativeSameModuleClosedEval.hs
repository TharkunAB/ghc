{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Main (main) where

import Data.List (foldl')
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)

fib :: Natural -> Natural
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

sieve :: Natural -> [Natural]
sieve limit = go [2 .. limit]
  where
    go [] = []
    go (p : xs) = p : go [x | x <- xs, x `mod` p /= 0]

sumViaInt :: [Natural] -> Natural
sumViaInt xs =
  let total :: Int
      total = foldl' (\acc x -> acc + fromIntegral x) 0 xs
  in fromIntegral total

stepNat :: Natural -> Natural
stepNat n =
  let k :: Int
      k = fromIntegral n * 2 + 1
  in fromIntegral k

evenNat :: Natural -> Bool
evenNat 0 = True
evenNat n = oddNat (n - 1)

oddNat :: Natural -> Bool
oddNat 0 = False
oddNat n = evenNat (n - 1)

data LocalPair = MkLocalPair Natural Natural

swapLocalPair :: LocalPair -> LocalPair
swapLocalPair (MkLocalPair x y) =
  let delta :: Int
      delta = fromIntegral x + 0
  in MkLocalPair y (fromIntegral delta)

fib10Proof :: fib 10 :~: 55
fib10Proof = Refl

fib8Proof :: fib 8 :~: 21
fib8Proof = Refl

sieve20Proof :: sieve 20 :~: '[2, 3, 5, 7, 11, 13, 17, 19]
sieve20Proof = Refl

sumViaIntProof :: sumViaInt '[10, 20, 25] :~: 55
sumViaIntProof = Refl

mapCallbackProof :: Prelude.map stepNat '[1, 2, 3, 4] :~: '[3, 5, 7, 9]
mapCallbackProof = Refl

mutualEvenProof :: evenNat 12 :~: 'True
mutualEvenProof = Refl

mutualOddProof :: oddNat 12 :~: 'False
mutualOddProof = Refl

localDataProof :: swapLocalPair ('MkLocalPair 3 7) :~: 'MkLocalPair 7 3
localDataProof = Refl

main :: IO ()
main =
  case ( fib10Proof
       , fib8Proof
       , sieve20Proof
       , sumViaIntProof
       , mapCallbackProof
       , mutualEvenProof
       , mutualOddProof
       , localDataProof
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> pure ()
