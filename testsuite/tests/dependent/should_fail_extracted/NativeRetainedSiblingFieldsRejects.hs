{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module NativeRetainedSiblingFieldsRejects where

import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..), toList, zipVec)
import SiblingColumns (Columns(..), flags, letters, size)
import TypeLambda (Fields(..), Map)

wrongPairs :: Columns -> Columns -> [(Char, Bool)]
wrongPairs p q = toList (zipVec (letters p) (flags q))

wrongRow :: foreach (p :: Columns) -> foreach (q :: Columns) ->
  Fields (Map (\a -> Vec a (size p)) '[Char, Bool])
wrongRow p q = Field (letters p) (Field (flags q) End)

wrongKnown :: size ('Columns 'Z 'VNil 'VNil) :~: 'S 'Z
wrongKnown = Refl

wrongFlags :: foreach (p :: Columns) -> Vec Bool ('S (size p))
wrongFlags (Columns _ _ bs) = bs
