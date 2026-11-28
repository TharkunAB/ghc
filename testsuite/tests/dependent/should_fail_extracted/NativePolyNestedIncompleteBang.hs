{-# LANGUAGE BangPatterns, DataKinds, DependentTypes, ListTuplePuns #-}
module NativePolyNestedIncompleteBang (badPartial, badBang) where

import Data.Type.Equality ((:~:) (Refl))

partialSum :: Either (Either a b) c -> Either a (Either b c)
partialSum (Left (Left x)) = Left x
partialSum (Right z)       = Right (Right z)

badPartial :: foreach (x :: a) -> partialSum ('Left ('Left x)) :~: 'Left x
badPartial _ = Refl

strictPairR :: (a, b) -> c -> (a, (b, c))
strictPairR (x, y) !z = (x, (y, z))

badBang :: foreach (x :: a) -> foreach (y :: b) -> foreach (z :: c) ->
  strictPairR '(x, y) z :~: '( x, '(y, z) )
badBang _ _ _ = Refl
