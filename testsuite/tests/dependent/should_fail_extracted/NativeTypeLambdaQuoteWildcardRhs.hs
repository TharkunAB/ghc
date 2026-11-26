{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TemplateHaskell, TypeFamilies #-}
module NativeTypeLambdaQuoteWildcardRhs where

import Data.Proxy (Proxy)
import Language.Haskell.TH (Dec, Q)

badWildcardCase :: Q [Dec]
badWildcardCase = [d|
  type BadCase = case ('Nothing :: Maybe Bool) of { _ -> Proxy _ }
  |]

badWildcardFamily :: Q [Dec]
badWildcardFamily = [d|
  type family BadFam (tag :: Maybe Bool) where
    BadFam ('Just _) = Proxy _
  |]
