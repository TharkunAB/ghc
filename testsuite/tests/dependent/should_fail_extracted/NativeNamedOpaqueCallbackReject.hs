{-# LANGUAGE NoDependentTypes #-}
module NativeNamedOpaqueCallbackReject where

import NativeNamedOpaqueCallbackLexical ()
import qualified NativeNamedOpaqueCallbackProvider as Other
import NativeNamedOpaqueCallbackPublic (called, owns, ownsCall, stored)

wrongOwner :: Bool
wrongOwner = owns Other.otherMapper (stored Other.otherMapper)

wrongCall :: Bool
wrongCall = ownsCall Other.otherMapper True (called True)
