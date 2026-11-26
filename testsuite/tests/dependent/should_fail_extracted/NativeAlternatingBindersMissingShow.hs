{-# LANGUAGE NoDependentTypes #-}
module NativeAlternatingBindersMissingShow where

import NativeAlternatingBindersProvider

data NoShow = NoShow

missingShow :: Maybe NoShow -> (String, Int)
missingShow index = showAfter True index NoShow (Reading 51)
