{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
module NativeGadtImplicitSourceWrongFail where

import qualified NativeGadtImplicitSourceDep as Dep
import qualified NativeGadtImplicitSourceNoReq as NoReq

badDep :: Dep.Edge 'Dep.Tip ('Dep.Fork 'Dep.Tip)
badDep = Dep.Step

badNoReq :: NoReq.Edge 'NoReq.Tip ('NoReq.Fork 'NoReq.Tip)
badNoReq = NoReq.Step
