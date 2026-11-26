{-# LANGUAGE GADTs, NoDependentTypes, TemplateHaskell #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaQuoteHelpers (shadowed, wildcardEvidence)
import NativeTypeLambdaTypedCallbacks (after, annotated, before, captured, named)
import NativeTypeLambdaTypedRuntime (ordinary)
import TypeLambda (Fields(..))
import TypeLambdaQuote (checkedColumns, labelledColumn)

main :: IO ()
main = do
  case wildcardEvidence of
    (Refl, Refl, Refl, Refl) -> pure ()
  case (checkedColumns, labelledColumn 'a') of
    (Field count (Field flag End), Field label End) ->
      print (count, flag, label, shadowed 'b')
  print ($$(named), $$(before), $$(after), $$(annotated), $$(captured) "checked", ordinary)
