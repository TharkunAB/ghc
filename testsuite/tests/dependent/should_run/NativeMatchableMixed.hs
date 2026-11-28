{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, ScopedTypeVariables, TypeApplications #-}
module NativeMatchableMixed (present) where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import MatchableEquality (project)
import MatchableView (Applied(..), Known(..))
import RetainedCharacters (remember, rememberedChar)

{-# INLINE present #-}
present :: Applied (Maybe Char) -> Maybe Int -> (Char, Maybe Bool)
present (Applied (_ :: Proxy f) KnownM (_ :: x) run) n =
  (rememberedChar (remember 'λ'), project @f @x Refl run n)
