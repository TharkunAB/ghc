{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
module Main where

import NativeLocalBooleanResultLib

step :: Step 'Z ('Nest ('Var 'Z)) 'Done ('Nest 'Done)
step = Reduce

other :: Step ('S 'Z) ('Var 'Z) 'Done ('Var 'Z)
other = Reduce

main :: IO ()
main = case (step, other) of (Reduce, Reduce) -> putStrLn "checked substitution"
