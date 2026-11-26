{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies #-}
module NativeDataFamilyPromoWrongCoercion where
import Data.Type.Coercion (Coercion(Coercion))
import TupleProvider
wrongCoercion :: Coercion ('Tup2 'True 'False) ('Tup2 'True 'True :: Tuple Bool Bool)
wrongCoercion = Coercion
