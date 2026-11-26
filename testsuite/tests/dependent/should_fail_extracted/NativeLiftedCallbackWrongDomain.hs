{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, ScopedTypeVariables #-}
module NativeLiftedCallbackWrongDomain where
import CompareAt
import Data.Proxy
newtype Bit = Bit Bool

badInput :: (Bit -> Bool -> Ordering) -> Bool -> Bool -> IO ()
badInput f x y = print (keepFamilyCompare f x y)

badResult :: (Bool -> Bool -> Bool) -> Bool -> Bool -> IO ()
badResult f x y = print (keepFamilyCompare f x y)

badAbstract :: forall tag. Proxy tag -> (Bool -> Result tag) -> Bool -> Bool -> IO ()
badAbstract _ f x y = print (keepFamilyCompare f x y)
