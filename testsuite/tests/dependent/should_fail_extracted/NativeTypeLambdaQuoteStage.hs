{-# LANGUAGE NoDependentTypes, TemplateHaskell #-}
module NativeTypeLambdaQuoteStage where

import Language.Haskell.TH.Syntax (Code, Q)

localCode :: Code Q Int
localCode = [|| 42 ||]

badStage :: Int
badStage = $$(localCode)
