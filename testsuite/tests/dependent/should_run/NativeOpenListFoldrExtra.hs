{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, Modifiers,
             PolyKinds, ScopedTypeVariables, StandaloneKindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenListFoldrExtra (userFoldOk) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeOpenListFoldrProducer as F
import PreludeMapLengthSize (Nat(..), plus)

type FoldDirect :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type FoldDirect k z xs = F.direct k z xs

type FoldLocal :: forall a b. (a %U -> b %U -> b) -> b -> [a] -> b
type FoldLocal k z xs = F.local k z xs

directNil :: forall a b (k :: a %U -> b %U -> b) (z :: b).
             Proxy k -> Proxy z -> FoldDirect k z '[] :~: z
directNil _ _ = Refl

directCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
              Proxy k -> Proxy z -> Proxy x -> Proxy xs
           -> FoldDirect k z (x ': xs) :~: k x (FoldDirect k z xs)
directCons _ _ _ _ = Refl

localCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
             Proxy k -> Proxy z -> Proxy x -> Proxy xs
          -> FoldLocal k z (x ': xs) :~: k x (FoldLocal k z xs)
localCons _ _ _ _ = Refl

retained :: foreach (n :: Nat) -> n :~: n
retained _ = Refl

retainedFold :: foreach (xs :: [Nat]) -> F.direct plus 'Zero xs :~: F.direct plus 'Zero xs
retainedFold xs = retained (F.direct plus Zero xs)

userFoldOk :: Bool
userFoldOk =
  let _k = (directNil, directCons, localCons)
  in case retainedFold [Zero] of Refl -> True
