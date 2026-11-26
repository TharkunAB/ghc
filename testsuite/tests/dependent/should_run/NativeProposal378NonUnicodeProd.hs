{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeProposal378NonUnicodeProd ((∏), prodSample) where
(∏) :: [Int] -> [Int] -> [Int]
xs ∏ ys = xs ++ ys
prodSample :: [Int]
prodSample = [1] ∏ [2]
