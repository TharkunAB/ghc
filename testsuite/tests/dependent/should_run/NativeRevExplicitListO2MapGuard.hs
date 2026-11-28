{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeRevExplicitListO2MapGuard (mapAppendEq, checkMapAppend) where

import Data.Type.Equality ((:~:)(Refl), gcastWith)

mapAppendEq :: forall a b. foreach (f :: a -> b) -> foreach (xs :: [a]) -> foreach (ys :: [a])
            -> map f (xs ++ ys) :~: (map f xs ++ map f ys)
mapAppendEq _ []       _  = Refl
mapAppendEq f (_ : xs) ys = gcastWith (mapAppendEq f xs ys) Refl

checkMapAppend :: Bool
checkMapAppend = case mapAppendEq not [True] [False] of Refl -> True
