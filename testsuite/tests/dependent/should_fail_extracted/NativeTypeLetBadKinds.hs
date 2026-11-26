{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeTypeLetBadKinds where
import GHC.TypeNats (Nat)

type UnusedCase = let ignored = case 'True of { True -> Maybe 'True; False -> Int } in Bool
type UnusedInvalid (a :: Bool) = let b = Maybe a in Int
type UnusedGroup (a :: Bool) = let { b = a; ignored = Maybe b } in Int
type SignedKind = let { width :: Bool; width = 0 } in Int
type IgnoredNonPair = let (_, _) = Bool in Int
type BadPairRhs = let (value, _) = '(Maybe 'True, 'False) in value
type BadPairOperand = let (flag, _) = '( 'True, 'False) in Maybe flag
type NestedWrongKind (dims :: (Bool, Nat)) = let ((_, _), height) = dims in height
