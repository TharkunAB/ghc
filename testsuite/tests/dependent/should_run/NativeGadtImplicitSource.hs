{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications #-}
module NativeGadtImplicitSource where

import qualified NativeGadtImplicitSourceDep as Dep
import qualified NativeGadtImplicitSourceNoReq as NoReq
import qualified NativeGadtImplicitSourceExplicit as Exp
import qualified NativeGadtImplicitSourceOrdinary as Ord

edgeDep :: Dep.Edge ('Dep.Fork 'Dep.Tip) 'Dep.Tip
edgeDep = Dep.Step

edgeNoReq :: NoReq.Edge ('NoReq.Fork 'NoReq.Tip) 'NoReq.Tip
edgeNoReq = NoReq.Step

edgeExp :: Exp.Edge 'Exp.Tip ('Exp.Fork 'Exp.Tip)
edgeExp = Exp.Step @'Exp.Fork @'Exp.Tip

edgeOrd :: Ord.Edge 'Ord.Tip ('Ord.Fork 'Ord.Tip)
edgeOrd = Ord.Step @'Ord.Tip @'Ord.Fork
