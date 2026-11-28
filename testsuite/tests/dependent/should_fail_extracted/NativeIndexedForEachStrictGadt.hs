{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
module NativeIndexedForEachStrictGadt where
import NativeIndexedForEachCtorProvider (Nat(..))
data StrictUTerm (n :: Nat) where
  SULam :: !(StrictUTerm ('Succ n)) -> StrictUTerm n
data BadWrap (t :: StrictUTerm n) where
  MkBad :: foreach (t :: StrictUTerm n) -> BadWrap t
