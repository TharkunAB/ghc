{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeApplications #-}
module NativeTypeLambdaTypeAppCaptureConflict where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import TypeLambda (Fields)
import TypeLambdaArgument (rowFor)

badSpliceCapture :: forall err. Proxy err -> Fields '[Either String Int, Either String Bool]
badSpliceCapture _ = rowFor @($([t| \tag cell -> Either tag cell |]) err) Proxy (Right 42) (Right True)

badCapturedConflict :: forall (err :: Type) (cell :: Type).
  Proxy err -> Proxy cell -> Fields '[Either String Int, Either String Bool]
badCapturedConflict _ _ = $([| rowFor @(\cell -> Either err cell) Proxy (Right 42) (Right True) |])
