{-# LANGUAGE ExtendedLet, ExplicitForAll, KindSignatures #-}
module NativeStandaloneExtendedLet (polySyn) where
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

polySyn :: forall (k :: Type) (a :: k). Proxy a -> (Proxy a, Proxy a)
polySyn p = res
  where
    type ProxyPair (x :: k) = (Proxy x, Proxy x)
    res :: ProxyPair a
    res = (p, p)
