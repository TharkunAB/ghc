{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fomit-interface-pragmas #-}
module NativeMapWitnessLib (NegateAll, First(..), Checked(..), checked) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import qualified Prelude as P

type NegateAll xs = P.map P.not xs

cons :: Proxy xs -> NegateAll ('P.True ': xs) :~: ('P.False ': NegateAll xs)
cons _ = Refl

data First (xs :: [P.Bool]) where
  FalseHead :: First ('P.False ': xs)
  TrueHead  :: First ('P.True ': xs)

data Checked xs where
  Checked :: xs :~: ys -> First ys -> Checked xs

checked :: Checked (NegateAll '[ 'P.True])
checked = Checked (cons (Proxy :: Proxy '[])) FalseHead
