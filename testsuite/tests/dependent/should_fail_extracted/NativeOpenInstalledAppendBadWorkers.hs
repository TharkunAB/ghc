{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -O2 -fwrite-source-equations -Wno-inline-rule-shadowing #-}
module NativeOpenInstalledAppendBadWorkers (forceRight, ruledAppend) where

forceRight :: [a] -> [a] -> [a]
forceRight [] ys = ys
forceRight (x : xs) ys = ys `seq` (x : forceRight xs ys)

{-# NOINLINE [2] ruledAppend #-}
ruledAppend :: [a] -> [a] -> [a]
ruledAppend [] ys = ys
ruledAppend (x : xs) ys = x : ruledAppend xs ys

{-# RULES "drop appended list" [~1] forall xs ys. ruledAppend xs ys = xs #-}
