{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module LegacyReader where
import NativeNonuniformGadtFieldsProvider

observesNext :: Swap Bool Bool -> Bool
observesNext term = case inspectSwap term of
    Stopped -> False
    Advanced -> True
