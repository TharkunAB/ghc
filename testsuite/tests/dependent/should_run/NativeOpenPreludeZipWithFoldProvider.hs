{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -fwrite-source-equations #-}
module NativeOpenPreludeZipWithFoldProvider (zipProbe, foldZip, pairStep) where

{-# NOINLINE [1] zipProbe #-}
zipProbe :: [a] -> [b] -> [(a, b)]
zipProbe [] _ = []
zipProbe (_ : _) [] = []
zipProbe (x : xs) (y : ys) = (x, y) : zipProbe xs ys

{-# INLINE [0] foldZip #-}
foldZip :: (a -> b -> r -> r) -> r -> [a] -> [b] -> r
foldZip k seed = go
  where
    go [] _ = seed
    go (_ : _) [] = seed
    go (a : as) (b : bs) = k a b (go as bs)

{-# INLINE [0] pairStep #-}
pairStep :: ((a, b) -> r -> s) -> a -> b -> r -> s
pairStep k x y r = k (x, y) r
