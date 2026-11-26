{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Specialize where
import qualified Data.Char
import Data.Type.Equality ((:~:) (Refl))
import UnicodeExport (Title)

same :: Title 'x' :~: Data.Char.toTitle 'x'
same = Refl
