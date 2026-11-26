{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE NoDependentTypes, TypeFamilies, UndecidableInstances #-}
module NativeAssociatedPayloadSigRejects where

import AssociatedPayloadSum (PayloadFormat (Packet))
import ComposedPayloadSum (Distinct, First (..), Repeated, Second (..))

missingTypeDomain :: Packet [Int] -> Bool
missingTypeDomain _ = False

class MissingFirst a
instance {-# OVERLAPPING #-} MissingFirst Int => First [Int] where
  type FirstBody [Int] False = Int
  type FirstBody [Int] True = Char

class MissingSecond a
instance {-# OVERLAPPING #-} MissingSecond Int => Second [Int] where
  type SecondBody [Int] False = String
  type SecondBody [Int] True = Bool

readDistinct :: Distinct -> Bool
readDistinct _ = True

readRepeated :: Repeated -> Bool
readRepeated _ = True
