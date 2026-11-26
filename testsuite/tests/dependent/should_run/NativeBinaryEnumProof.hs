{-# LANGUAGE DataKinds, DependentTypes, TypeOperators #-}
module NativeBinaryEnumProof where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeBinaryEnumCert as C
import NativeBinaryEnumPrefix

skipped :: Skipped b :~: 'False
skipped = Refl

carried :: Carried b :~: b
carried = Refl

unary :: Unary :~: 'True
unary = Refl

bothFalse :: BothFalse b :~: b
bothFalse = Refl

strictFalse :: StrictFalse b :~: b
strictFalse = Refl

saturated :: C.choose 'False 'False :~: 'False
saturated = Refl
