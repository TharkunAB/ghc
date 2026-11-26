module PublicVector
    (CharVector, fromChar, storedLength, storedCells, Nat(..), Vec, natToInt, toList) where

import PackedVecLibrary (Nat(..), Vec(..), natToInt, toList)
import RetainedCharVector (CharVector(..))
import RetainedCharLength (storedLength)
import RetainedCharProjection (storedCells)

fromChar :: Char -> CharVector
fromChar c = CharVector (S Z) (VCons c VNil)
