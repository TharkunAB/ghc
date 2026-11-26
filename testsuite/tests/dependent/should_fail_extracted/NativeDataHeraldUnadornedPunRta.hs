{-# LANGUAGE DependentTypes #-}
module NativeDataHeraldUnadornedPunRta where
import Data.Proxy (Proxy (Proxy))
import NativeDataHeraldInTypesProvider (Nat (..))
idNatProxy :: forall (n :: Nat) -> Proxy n
idNatProxy _ = Proxy
bad :: foreach (x :: Nat) -> Proxy (data x)
bad x = check (4 :: Int)
  where
    check :: forall x. x -> Proxy (data x)
    check _ = idNatProxy x
