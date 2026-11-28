{-# LANGUAGE NoDependentTypes #-}
module Main where
import CertifiedSplitClient
main :: IO ()
main = do
    splitObservation >>= print
    checkDemand >>= print
    checkedObservation >>= print
    print checkBounds
    pairedObservation >>= print
    mixedObservation >>= print
