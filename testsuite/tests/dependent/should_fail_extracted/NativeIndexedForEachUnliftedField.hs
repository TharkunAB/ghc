{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, MagicHash #-}
module NativeIndexedForEachUnliftedField where
import GHC.Exts (Int#)
import NativeIndexedForEachCtorProvider (Nat(..))
data UnlGadt (n :: Nat) where
  MkUnl :: Int# -> UnlGadt 'Zero
data BadUnl where
  MkBadUnl :: foreach (u :: UnlGadt 'Zero) -> BadUnl
