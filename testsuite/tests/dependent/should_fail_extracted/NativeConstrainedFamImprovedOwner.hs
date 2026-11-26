{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, NoDependentTypes, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
module NativeConstrainedFamImprovedOwner where
import Data.Proxy (Proxy(..))
import NativeConstrainedFamilyProvider
instance {-# OVERLAPPING #-} Allowed a => Output (Key [a]) [a] where
    type Project (Key [a]) = Bool
    type Explicit (Key [a]) [a] = Bool
aliased :: Proxy (Projected [Int], Projected Char)
aliased = Proxy
missingOwner :: forall c. Proxy c -> Proxy (Project c)
missingOwner _ = Proxy
wrongExplicit :: Proxy c -> Proxy (Explicit (Key Bool) Char)
wrongExplicit _ = Proxy
