{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
module NativeLocalBooleanResultFail where

import NativeLocalBooleanResultLib

step :: Step 'Z ('Nest ('Var 'Z)) 'Done ('Nest ('Var 'Z))
step = Reduce
