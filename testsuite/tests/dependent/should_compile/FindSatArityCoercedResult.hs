{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, FunctionalDependencies, TypeFamilies, UndecidableInstances #-}
module FindSatArityCoercedResult where

class C a b r | a b -> r where
  op :: a -> b -> r

instance (r ~ (c -> d)) => C (d -> d) (c -> d) r where
  (op f g) c = f (g c)
  {-# INLINE op #-}

type family Res a
type instance Res Int = Bool -> Bool

coercedFam :: (Res a ~ (Bool -> Bool)) => (Bool -> Bool) -> Res a
coercedFam f x = f x
{-# INLINE coercedFam #-}

useOp :: Int -> Int
useOp = op (+ (1 :: Int)) (* (2 :: Int))
