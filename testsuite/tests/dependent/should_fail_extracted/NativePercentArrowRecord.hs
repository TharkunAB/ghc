{-# LANGUAGE Modifiers, NoDependentTypes #-}
module NativePercentArrowRecord where
import GHC.Exts (M)
data R = R { unR %M :: Int }
