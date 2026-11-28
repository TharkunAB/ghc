{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies #-}
module NativeDataFamilyPromoWrongRefinement where
import TupleProvider
wrongRefinement :: At ('Refines :: C (Char, Bool))
wrongRefinement = At
