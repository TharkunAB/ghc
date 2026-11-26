{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies, TypeOperators #-}
module NativeDataFamilyPromoWrongProof where
import Data.Type.Equality ((:~:)(Refl))
import TupleProvider
wrongProof :: 'Tup2 'True 'False :~: ('Tup2 'True 'True :: Tuple Bool Bool)
wrongProof = Refl
