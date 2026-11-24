{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
-- | List reversal using an explicit singleton bracket literal under -O2 rules.
module ExplicitListReverse (app, rev) where

app :: [a] -> [a] -> [a]
app []       ys = ys
app (x : xs) ys = x : app xs ys

rev :: [a] -> [a]
rev []       = []
rev (x : xs) = app (rev xs) [x]
