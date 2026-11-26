{-# LANGUAGE ScopedTypeVariables, NoDependentTypes #-}
module NativeNamedCallbackChangeSizeRule () where
import qualified PreludeMapLengthSize as A

{-# RULES "size/changed" forall (xs :: [Bool]). A.size xs = A.Zero #-}
