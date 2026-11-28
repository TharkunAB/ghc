{-# LANGUAGE DependentTypes #-}
module NativeProposal378NoWriteSourceEqs where
import Data.Type.Equality ((:~:)(Refl))
import NativeProposal378NoWriteSourceEqs_Prov
closureEq :: forall g a b (s :: Slot g a).
             replace (Bind @(Push g a) @b (At (Old New))) (At s) :~: Bind (At (Old s))
closureEq = Refl
