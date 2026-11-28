{-# LANGUAGE GADTs, NoDependentTypes, NoLiberalTypeSynonyms, PolyKinds #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module NativeSynonymReifyRejects where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M)
import NativeSynonymReifyClient (Copied, CopiedPlain)

badExplicit :: Copied @M Maybe Bool :~: Maybe Bool
badExplicit = Refl

badPlain :: CopiedPlain Maybe Bool :~: Maybe Bool
badPlain = Refl
