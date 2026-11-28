{-# LANGUAGE BangPatterns, NoDependentTypes #-}
module NativeBinaryEnumCert where

flipBit :: Bool -> Bool
flipBit False = True
flipBit True = False

choose :: Bool -> Bool -> Bool
choose False b = b
choose True _ = False

bothDemanded :: Bool -> Bool -> Bool
bothDemanded False b = b
bothDemanded True b = case b of
  True -> False
  False -> True

secondFirst :: Bool -> Bool -> Bool
secondFirst a False = a
secondFirst _a True = False

explicitStrict :: Bool -> Bool -> Bool
explicitStrict False b = b
explicitStrict True !_b = False

three :: Bool -> Bool -> Bool -> Bool
three False b _c = b
three True _b _c = False

{-# NOINLINE guarded #-}
guarded :: Bool -> Bool -> Bool
guarded False b = b
guarded True _ = False

{-# RULES "guarded/true" forall b. guarded True b = True #-}
