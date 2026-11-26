{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, StandaloneKindSignatures, TypeFamilies #-}
module NativeDataFamilyPromoDisabled where
import TupleProvider
pairIndex :: At ('Tup2 'True 'False :: Tuple Bool Bool)
pairIndex = At
