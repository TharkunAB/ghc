{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeNamedOpaqueCharacterReject where

import CharacterDecision (Decision, Space, spaceDecision)
import qualified Data.Char as Char
import Data.Type.Equality ((:~:) (Refl))

wrong :: Space 'x' :~: 'False
wrong = Refl

mixed :: Decision Char.isSpace (Char.isSpace 'x')
mixed = spaceDecision 'x'
