{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module NativeLiftedCallbackSwapped where
import CompareAt (Result, Witness(..))
import Data.Kind (Type)

type family Via (tag :: Bool) :: Type where
    Via tag = Result tag

swappedVia :: foreach (cmp :: Bool -> Via 'True)
           -> foreach (x :: Bool) -> foreach (y :: Bool) -> Witness (cmp x y)
swappedVia cmp x y = case cmp y x of
    LT -> Less
    EQ -> Equal
    GT -> Greater
