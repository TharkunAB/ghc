{-# LANGUAGE NoDependentTypes #-}
module Main where
import ForgetWitness

main :: IO ()
main = print (map (observe . forget . retained) [True, False],
              observe (forget (unknown (error "forced witness"))))
