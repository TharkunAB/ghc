{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TemplateHaskell, TypeApplications #-}
module NativeTypeLambdaQuoteWrongKind where

import Data.Proxy (Proxy(..))
import Language.Haskell.TH.Syntax (Code, Q)

wrongKind = $([| Proxy :: Proxy ((\a -> Either 'True a) Bool) |])

badTypedKind :: Code Q (Either String Int)
badTypedKind = [|| id @((\cell -> Either 'True cell) Int) (Right 42) ||]
