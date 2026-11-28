{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, ListTuplePuns, TypeFamilies #-}
module NativeRetainedFamDomWrongTag where
import ConstrainedElements (Collects(..), FieldKey)
import Data.Proxy (Proxy(..))
different :: foreach (a :: Bool) -> foreach (b :: Bool)
          -> Collects (FieldKey a) => Proxy '(a, Elem (FieldKey b))
different _ _ = Proxy
