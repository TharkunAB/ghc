{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies #-}
module NativeDataFamilyOperandWrongIndex where
import RetainedTuple (keep)
import TupleProvider
wrong :: At ('Tup2 'True 'False)
wrong = keep (Tup2 False True)
