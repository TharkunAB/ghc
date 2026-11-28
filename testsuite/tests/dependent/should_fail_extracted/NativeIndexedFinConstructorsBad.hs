{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeIndexedFinConstructorsBad where
import Data.Proxy (Proxy)
import PackedVecLibrary (sourceLength)

data CompoundResult (xs :: [Bool]) where
    CompoundResult :: foreach (ys :: [Bool]) -> CompoundResult (True ': ys)

data TwoFieldsResult (xs :: [Bool]) where
    TwoFieldsResult :: foreach (ys :: [Bool]) (zs :: [Bool]) -> TwoFieldsResult ys

data WrongLen where
    WrongLen :: Proxy (sourceLength True) -> WrongLen
