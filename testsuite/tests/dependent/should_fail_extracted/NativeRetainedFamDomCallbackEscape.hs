{-# LANGUAGE DependentTypes, ListTuplePuns, RankNTypes, TypeFamilies #-}
module NativeRetainedFamDomCallbackEscape where
import ConstrainedElements (Collects(..))
import Data.Proxy (Proxy(..))
escape :: forall a. (Collects a => Proxy (Elem a)) -> Proxy (Elem a)
escape _ = Proxy
