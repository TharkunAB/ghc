{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeBinaryEnumRuleFail where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeBinaryEnumCert as C
import NativeBinaryEnumPrefix (Skipped)

{-# RULES "redirect" forall b. C.choose True b = True #-}

wrong :: Skipped b :~: 'False
wrong = Refl
