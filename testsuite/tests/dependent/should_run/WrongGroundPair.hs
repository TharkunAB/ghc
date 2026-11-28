{-# LANGUAGE DataKinds, NoDependentTypes, TypeApplications #-}
module WrongGroundPair where

import Data.Proxy (Proxy(..))
import PreludePairExport (first)

wrong = first (Proxy @'( 'True, 'x')) (Proxy @'False)
