{-# LANGUAGE DependentTypes, ListTuplePuns, MagicHash, TypeFamilies, UnboxedTuples #-}
module NativeUnboxedTupleAndCharPrimProvider
  ( EqCharLit
  , MatchCharVar
  , MatchCharWild
  , MatchCharAnnot
  , FstU
  , SndU
  , SwapU
  , PairSyn
  , charLitRefl
  , eqCharHit
  , eqCharMiss
  , eqCharOther
  , matchCharVarProof
  , matchCharWildProof
  , matchCharAnnotProof
  , fstUProof
  , sndUProof
  , swapUFstProof
  , swapUSndProof
  ) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (Char(C#), Char#)
import Numeric.Natural (Natural)

type EqCharLit :: Char -> Bool
type family EqCharLit c where
  EqCharLit ('C# 'a'#) = 'True
  EqCharLit ('C# 'b'#) = 'False
  EqCharLit _          = 'False

type MatchCharVar :: Char -> ()
type family MatchCharVar c where
  MatchCharVar ('C# x) = '()

type MatchCharWild :: Char -> Bool
type family MatchCharWild c where
  MatchCharWild ('C# _) = 'True

type MatchCharAnnot :: Char -> Natural
type family MatchCharAnnot c where
  MatchCharAnnot ('C# (x :: Char#)) = 1

type FstU :: (# a, b #) -> a
type family FstU p where
  FstU (# x, _ #) = x

type SndU :: (# a, b #) -> b
type family SndU p where
  SndU ('(# , #) _ y) = y

type SwapU :: (# a, b #) -> (# b, a #)
type family SwapU p where
  SwapU (# x, y #) = (# y, x #)

type PairSyn :: (# Bool, Char #)
type PairSyn = (# 'True, 'x' #)

charLitRefl :: 'C# 'a'# :~: 'a'
charLitRefl = Refl

eqCharHit :: EqCharLit 'a' :~: 'True
eqCharHit = Refl

eqCharMiss :: EqCharLit 'b' :~: 'False
eqCharMiss = Refl

eqCharOther :: EqCharLit 'z' :~: 'False
eqCharOther = Refl

matchCharVarProof :: MatchCharVar 'k' :~: '()
matchCharVarProof = Refl

matchCharWildProof :: MatchCharWild 'q' :~: 'True
matchCharWildProof = Refl

matchCharAnnotProof :: MatchCharAnnot 'm' :~: 1
matchCharAnnotProof = Refl

fstUProof :: FstU PairSyn :~: 'True
fstUProof = Refl

sndUProof :: SndU PairSyn :~: 'x'
sndUProof = Refl

swapUFstProof :: FstU (SwapU PairSyn) :~: 'x'
swapUFstProof = Refl

swapUSndProof :: SndU (SwapU PairSyn) :~: 'True
swapUSndProof = Refl
