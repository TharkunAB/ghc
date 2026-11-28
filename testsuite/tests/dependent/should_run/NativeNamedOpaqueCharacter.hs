{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeNamedOpaqueCharacter where

import CharacterDecision (Decision, Space, spaceDecision)
import qualified Data.Char as Char
import Data.Type.Equality ((:~:) (Refl))
import GHC.Matchability (U)

atX :: Decision Char.isSpace (Space 'x')
atX = spaceDecision 'x'

ascribed :: Space 'x' :~: (Char.isSpace :: Char %U -> Bool) 'x'
ascribed = Refl

computed :: Char.isSpace 'x' :~: 'False
computed = Refl
