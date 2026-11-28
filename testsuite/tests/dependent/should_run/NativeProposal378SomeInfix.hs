{-# LANGUAGE DependentTypes #-}
module NativeProposal378SomeInfix (someInfixVal) where
import Data.Kind (Type)
import GHC.Types (List)
import NativeProposal378SyntaxAndFlagsProvider (Nat, Vec, Pair2)
w :: forall (t :: Type) -> List Char
w _ = "ok"
someInfixVal :: List Char
someInfixVal = w (some (m n :: Nat). Pair2 (Vec Int m) (Vec Int n))
