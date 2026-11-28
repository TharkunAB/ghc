{-# LANGUAGE ImpredicativeTypes, NoDependentTypes #-}
module NativeSimplifierEscape where
import WitnessSchema (Payload(..), RankedBuilder)

escape :: RankedBuilder -> Bool -> a
escape builder b = case builder b of Payload value _ -> value
