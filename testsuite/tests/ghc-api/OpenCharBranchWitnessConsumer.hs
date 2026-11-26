{-# LANGUAGE NoDependentTypes #-}
module OpenCharBranchWitnessConsumer (probe) where

import OpenCharBranchWitnessProducer (classify)

probe :: Char -> Char
probe = classify
