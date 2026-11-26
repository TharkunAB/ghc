{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -fwrite-source-equations #-}
module NativeOpenPreludeZipWithBadFold (zipStrictSeed, foldZipStrict, pairStep) where

{-# NOINLINE [1] zipStrictSeed #-}
zipStrictSeed :: [a] -> [b] -> [(a, b)]
zipStrictSeed [] _ = []
zipStrictSeed (_ : _) [] = []
zipStrictSeed (x : xs) (y : ys) = (x, y) : zipStrictSeed xs ys

{-# INLINE [0] foldZipStrict #-}
foldZipStrict :: (a -> b -> r -> r) -> r -> [a] -> [b] -> r
foldZipStrict k seed xs ys = seed `seq` go xs ys
  where
    go [] _ = seed
    go (_ : _) [] = seed
    go (a : as) (b : bs) = k a b (go as bs)

{-# INLINE [0] pairStep #-}
pairStep :: ((a, b) -> r -> s) -> a -> b -> r -> s
pairStep k x y r = k (x, y) r
