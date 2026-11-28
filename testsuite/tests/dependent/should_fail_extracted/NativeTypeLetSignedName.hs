{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeLetSignedName where
type Bad = let { width :: Bool; height = 'True } in height
