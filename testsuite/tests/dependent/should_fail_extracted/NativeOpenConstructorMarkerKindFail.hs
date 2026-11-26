{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, KindSignatures, TypeOperators #-}
module NativeOpenConstructorMarkerKindFail where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenConstructorLargerWorker as L

wrongMarkerKind :: L.retain ('L.Marker :: L.Marker 'True) 'False :~: 'False
wrongMarkerKind = Refl
