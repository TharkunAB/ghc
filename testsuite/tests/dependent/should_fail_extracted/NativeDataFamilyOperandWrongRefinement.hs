{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies #-}
module NativeDataFamilyOperandWrongRefinement where
import TupleProvider
keepOther :: foreach (t :: C (Char, Bool)) -> At t
keepOther _ = At
wrongRefinement = keepOther Refines
