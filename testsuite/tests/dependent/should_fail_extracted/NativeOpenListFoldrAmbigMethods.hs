{-# LANGUAGE AllowAmbiguousTypes, NoDependentTypes #-}
{-# OPTIONS_GHC -O1 -fwrite-source-equations #-}
module NativeOpenListFoldrAmbigMethods (Ambiguous(..)) where

import qualified NativeOpenListFoldrProducer as F

class Ambiguous t where
  ambiguous :: (a -> b -> b) -> b -> [a] -> b
  ambiguousTag :: t a -> Bool

instance Ambiguous [] where
  ambiguous = F.direct
  ambiguousTag _ = True

instance Ambiguous Maybe where
  ambiguous _ z _ = z
  ambiguousTag _ = False
