{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
module CountOne where
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthSize (Nat(..))
import PreludeMapLengthSource (Count)

one :: Count '[ 'True] :~: 'Succ 'Zero
one = Refl
