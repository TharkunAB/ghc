{-# LANGUAGE DependentTypes #-}
module NativeExtendedLetMutualRecSyn where
bad :: Int
bad = let { type A = B; type B = A } in 0
