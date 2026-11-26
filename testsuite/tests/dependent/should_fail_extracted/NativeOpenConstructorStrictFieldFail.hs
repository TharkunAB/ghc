{-# LANGUAGE DataKinds, DependentTypes #-}
module NativeOpenConstructorStrictFieldFail where

import qualified NativeOpenConstructorStrictWorker as W

left :: forall a b. a -> b -> W.pick ('W.LeftFields a b)
left _ y = y
