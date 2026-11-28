{-# LANGUAGE DependentTypes, ListTuplePuns, RankNTypes, TypeFamilies #-}
module NativeRetainedFamDomCallbackMissing where
import ConstrainedElements (Collects(..))
import Data.Proxy (Proxy)
unqualified :: (forall c. Proxy c -> Elem c -> Elem c) -> Bool
unqualified _ = True
