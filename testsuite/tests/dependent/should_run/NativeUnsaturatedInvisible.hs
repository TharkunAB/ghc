{-# LANGUAGE NoDependentTypes #-}
module NativeUnsaturatedInvisible (checked) where

import InvisibleFamily (requiredArguments, wrappedValues)
import NativeUnsaturatedInvisibleConsumer
  ( carry
  , observed
  , observedVisible
  , readAtTwoKinds
  , readRequired
  )

checked :: Bool
checked = (carry, observed, observedVisible, readAtTwoKinds, readRequired, requiredArguments, wrappedValues) `seq` True
