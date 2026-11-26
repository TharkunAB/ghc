{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
module NativeMonomorphicRetainedProjectionRejects where

import PackedVecLibrary (Nat(..), Vec)
import RetainedCharLength (storedLength)
import RetainedCharVector (CharVector(..), cellsOf, lengthOf)

uncheckedReplace :: CharVector -> CharVector -> Maybe CharVector
uncheckedReplace target source = Just (CharVector (lengthOf target) (cellsOf source))

wrongIndex :: foreach (r :: CharVector) -> Vec Char ('S (storedLength r))
wrongIndex (CharVector _ xs) = xs
