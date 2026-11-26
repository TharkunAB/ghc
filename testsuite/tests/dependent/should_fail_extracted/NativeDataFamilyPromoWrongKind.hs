{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies #-}
module NativeDataFamilyPromoWrongKind where
import TupleProvider
wrongKind :: At ('Tup2 'True 'False :: Tuple Int Bool)
wrongKind = At
