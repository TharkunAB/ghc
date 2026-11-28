{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fomit-interface-pragmas #-}
module NativeOpenPreludeZipWithFoldProof (probeLengthZip) where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenPreludeZipWithFoldProvider (zipProbe)
import NativeOpenPreludeZipWithFoldRule ()
import PreludeMapLengthSize (Size, minSize, size)

probeLengthZip :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
               -> size (zipProbe xs ys) :~: minSize (size xs) (size ys)
probeLengthZip []       _        = Refl
probeLengthZip (_ : _)  []       = Refl
probeLengthZip (_ : xs) (_ : ys) =
  case probeLengthZip xs ys of
    Refl -> Refl
