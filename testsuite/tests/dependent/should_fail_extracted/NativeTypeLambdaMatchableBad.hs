{-# LANGUAGE DependentTypes #-}
module NativeTypeLambdaMatchableBad where
import Data.Kind (Type)
import GHC.Exts (M)
type Bad = ((\x -> x) :: Type %M -> Type)
