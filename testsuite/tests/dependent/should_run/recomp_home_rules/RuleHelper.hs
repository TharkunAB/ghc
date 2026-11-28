{-# LANGUAGE Trustworthy #-}
module RuleHelper (value, delayed) where

import FoldProvider (fold)

value :: Int
value = 7

{-# NOINLINE private #-}
private :: (Int, Int)
private = (1, 2)

{-# NOINLINE lateStep #-}
lateStep :: Bool -> Bool -> Bool
lateStep _ result = result

{-# INLINE delayed #-}
delayed :: Bool -> [Bool] -> Bool
delayed seed xs = fold lateStep seed xs
