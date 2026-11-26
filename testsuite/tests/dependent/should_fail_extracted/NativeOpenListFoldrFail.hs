{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, FlexibleContexts, GADTs,
             Modifiers, PolyKinds, ScopedTypeVariables, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -Wno-simplifiable-class-constraints #-}
module NativeOpenListFoldrFail where

import Data.Kind (Constraint)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import ListFoldrSource (Folded)
import Prelude (Bool)
import qualified Prelude as P
import PreludeAppendLengthSource (Appended)
import PreludeMapLengthProof (Spine(..))

wrongCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
             Proxy k -> Proxy z -> Proxy x -> Proxy xs
          -> Folded k z (x ': xs) :~: z
wrongCons _ _ _ _ = Refl

missingInduction :: Proxy k -> Proxy z -> Spine xs -> Proxy ys
                 -> Folded k z (Appended xs ys) :~: Folded k (Folded k z ys) xs
missingInduction _ _ Empty _ = Refl
missingInduction _ _ (Cons _) _ = Refl

data Dict (p :: Constraint) where
  Dict :: p => Dict p

masked :: Dict (P.Foldable []) -> ()
masked Dict =
  let nil :: forall a b (k :: a %U -> b %U -> b) (z :: b).
             Proxy k -> Proxy z -> P.foldr k z ('[] :: [a]) :~: z
      nil _ _ = Refl
  in ()
