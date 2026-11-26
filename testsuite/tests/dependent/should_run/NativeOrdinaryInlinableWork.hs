{-# LANGUAGE BangPatterns, DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeOrdinaryInlinableWork (tally, constant, lazyValue) where

type family Payload (flag :: Bool) where
    Payload True = Int
    Payload False = ()

{-# NOINLINE bump #-}
bump :: foreach (flag :: Bool) -> Int -> Payload flag
bump True n = n + 2
bump False _ = ()

{-# INLINABLE tally #-}
tally :: Num a => (Int, Int) -> a -> a
tally (n, c) v
    | n <= 0 = v
    | otherwise = tally (n - 1, c) $! v + fromIntegral (bump True c + n)

{-# INLINABLE constant #-}
constant :: Int
constant = bump True 40

{-# NOINLINE bumpEq #-}
bumpEq :: foreach (flag :: Bool) -> ((flag ~ flag) => Int) -> Int
bumpEq _ n = n + 2

{-# INLINABLE [0] lazyValue #-}
lazyValue :: Int
lazyValue = bumpEq True 40
