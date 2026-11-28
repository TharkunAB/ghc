{-# LANGUAGE DependentTypes, MagicHash, TypeFamilies, UnboxedTuples #-}
module NativeUnboxedTupleNoPuns (FstUNP, SndUNP, p1, p2) where
import Data.Type.Equality ((:~:)(Refl))

type FstUNP :: (# a, b #) -> a
type family FstUNP p where
  FstUNP (# x, _ #) = x

type SndUNP :: (# a, b #) -> b
type family SndUNP p where
  SndUNP ((# , #) _ y) = y

p1 :: FstUNP (# True, 'x' #) :~: True
p1 = Refl

p2 :: SndUNP (# True, 'x' #) :~: 'x'
p2 = Refl
