{-# LANGUAGE ConstraintKinds, FlexibleContexts, MultiParamTypeClasses, NoDependentTypes, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module NativeConstrainedFamQualifiers where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import NativeConstrainedFamilyProvider (Collects(..), Display)
class Collects c => Self c x
withSelf :: Self c (Elem c) => Proxy c -> Bool
withSelf _ = True
acrossForall :: forall (c :: Type). Show (Elem c) => forall (x :: Type). Collects c => Proxy x -> Proxy c -> Bool
acrossForall _ _ = True
mentioned :: Proxy c -> Proxy (Display c)
mentioned _ = Proxy
