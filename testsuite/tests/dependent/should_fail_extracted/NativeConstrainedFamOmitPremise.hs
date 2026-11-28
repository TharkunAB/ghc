{-# LANGUAGE FlexibleContexts, NoDependentTypes, QuantifiedConstraints, ScopedTypeVariables, TypeFamilies #-}
module NativeConstrainedFamOmitPremise where
import Data.Proxy (Proxy(..))
import NativeConstrainedFamilyProvider (Allowed, Collects(..))
fromRule :: forall f a. (forall x. Allowed x => Collects (f x)) => Proxy (f a) -> ()
fromRule _ =
    let local :: Proxy (Elem (f a))
        local = Proxy
    in local `seq` ()
