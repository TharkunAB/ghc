{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedCastBadBetaTarget where
import NativeRetainedCastSourceCallProvider
badBeta :: forall a.
           foreach (body :: Term (CExt CEmpty a) a)
        -> foreach (m :: Term CEmpty a)
        -> Value m
        -> Progress (App (Lam body) m)
badBeta _ m w = PStep m (BetaLam w)
