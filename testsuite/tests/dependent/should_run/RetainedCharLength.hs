{-# LANGUAGE GADTs, NoDependentTypes #-}
module RetainedCharLength (storedLength) where

import PackedVecLibrary (Nat)
import RetainedCharVector (CharVector(..))

-- | Read the witness without demanding the vector.
storedLength :: CharVector -> Nat
storedLength (CharVector n _) = n
