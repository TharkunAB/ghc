{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module NativeTypeLambdaConflict where
import SchemaColumns (Checked)
import TypeLambda (Columns, Map)
type instance Columns (Checked String) fields = Map (\a -> Maybe a) fields
