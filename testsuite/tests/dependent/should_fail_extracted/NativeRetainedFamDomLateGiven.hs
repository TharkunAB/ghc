{-# LANGUAGE DataKinds, DependentTypes, FlexibleContexts, ListTuplePuns, TypeFamilies #-}
module NativeRetainedFamDomLateGiven where
import ConstrainedElements (Collects(..), FieldKey)
import Data.Proxy (Proxy(..))
earlier :: Proxy (Elem (FieldKey 'False))
        -> foreach (b :: Bool) -> Collects (FieldKey 'False) => Proxy b
earlier _ _ = Proxy
