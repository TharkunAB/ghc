{-# LANGUAGE DataKinds, NoDependentTypes, ScopedTypeVariables #-}
module NativeLiftedCallbackWrongTuple where
import NativeLiftedCallbackProvider (Decision, decide)

wrongTuple :: forall (chosen :: Bool). (Bool -> Bool) -> Bool -> (Decision chosen, ())
wrongTuple f x = (decide f x, ())
