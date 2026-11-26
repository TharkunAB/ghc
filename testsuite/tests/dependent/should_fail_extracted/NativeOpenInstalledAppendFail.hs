{-# LANGUAGE DataKinds, GADTs, KindSignatures, NoDependentTypes, PolyKinds,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenInstalledAppendFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PreludeAppendLengthSource (Appended, Plus)
import PreludeMapCompositionSource (ComposedMap)
import PreludeMapLengthProof (Spine(..))
import qualified PreludeMapLengthSize as S
import PreludeMapLengthSource (Count, Mapped)

type Empty = ('[] :: [Bool])

wrongEmpty :: Count (Appended Empty Empty) :~: 'S.Succ (Plus (Count Empty) (Count Empty))
wrongEmpty = Refl

missingAppendHyp :: Spine xs -> Proxy ys -> Count (Appended xs ys) :~: Plus (Count xs) (Count ys)
missingAppendHyp Empty _ = Refl
missingAppendHyp (Cons _) _ = Refl

missingMapHyp :: Proxy f -> Spine xs -> Proxy ys
              -> Mapped f (Appended xs ys) :~: Appended (Mapped f xs) (Mapped f ys)
missingMapHyp _ Empty _ = Refl
missingMapHyp _ (Cons _) _ = Refl

missingCompHyp :: Proxy f -> Proxy g -> Spine xs
               -> Mapped f (Mapped g xs) :~: ComposedMap f g xs
missingCompHyp _ _ Empty = Refl
missingCompHyp _ _ (Cons _) = Refl
