{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeIndexedFinMissingSucc where
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin(..))

missingSucc :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) -> a
missingSucc (x : _) FZ = x
