{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeLetPatterns where
import GHC.TypeNats (Nat)
import qualified NativeTypeLetPairBindings as P

data Choice = Some Nat Nat | None
type Refutable = let Some _ _ = 'Some 2 3 in Bool
type WrongProdArity = let P.Grid _ = 'P.Grid 2 3 in Bool

badValArity :: P.Grid -> Bool
badValArity (P.Grid _) = True
