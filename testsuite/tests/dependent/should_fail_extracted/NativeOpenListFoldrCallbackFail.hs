{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers,
             StandaloneKindSignatures #-}
module NativeOpenListFoldrCallbackFail where

import GHC.Exts (U)
import qualified NativeOpenListFoldrProducer as F

newtype WrappedBool = WrappedBool Bool

type BadFoldBits :: (WrappedBool %U -> Bool %U -> Bool) -> Bool -> [Bool] -> Bool
type BadFoldBits k z xs = F.foldBits k z xs
