{-# LANGUAGE DependentTypes #-}
module NativeProposal378BadVisBraces where
bad :: foreach {n :: Bool} -> Bool
bad b = b
