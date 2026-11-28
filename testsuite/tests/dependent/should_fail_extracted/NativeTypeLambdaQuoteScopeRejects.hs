{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TemplateHaskell, TypeApplications #-}
module NativeTypeLambdaQuoteScopeRejects where

import Data.Proxy (Proxy)
import Language.Haskell.TH (Dec, Q, Type)
import Language.Haskell.TH.Syntax (Code)

badWildcardScope :: Q [Dec]
badWildcardScope = [d|
  type BadScope = \tag -> case tag of { Just field -> Proxy field; _ -> Proxy field }
  |]

badQuoteScope :: Q Type
badQuoteScope = [t| ((\field -> field) Bool, field) |]

badTypedScope :: Code Q (Either String Int)
badTypedScope = [|| id @((\cell -> Either missing cell) Int) (Right 42) ||]
