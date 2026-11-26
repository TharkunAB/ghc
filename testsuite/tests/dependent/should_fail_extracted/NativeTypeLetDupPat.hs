{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeLetDupPat where
type PairDup = let (x, x) = '(Bool, Char) in x
type NestedDup = let ((x, _), x) = '( '(Bool, Char), Int) in x
