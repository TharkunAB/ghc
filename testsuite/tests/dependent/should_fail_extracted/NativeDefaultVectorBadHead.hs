{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications #-}
module NativeDefaultVectorBadHead where
import DefaultVector
import PackedVecLibrary

instance DefaultElement Char where
  defaultElement = 'x'

safeHead :: Vec a ('S n) -> a
safeHead (VCons x _) = x

bad :: Char
bad = safeHead (filled @Char 0)
