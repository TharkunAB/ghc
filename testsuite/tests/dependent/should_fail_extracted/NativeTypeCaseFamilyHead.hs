{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeTypeCaseFamilyHead where
type family G where G = Bool
type F a = case a of { G -> Int }
