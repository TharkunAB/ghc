{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
module Proof where
import CharacterExport (Flag)
import AliasExport (Alias, Reexport, LocalFlag, LocalUpper)
import Data.Type.Equality ((:~:) (Refl))
literal :: Flag 'x' :~: 'True
literal = Refl
aliasLiteral :: Alias 'x' :~: 'True
aliasLiteral = Refl
reexported :: Reexport '\xe9' :~: 'True
reexported = Refl
fallback :: Alias '!' :~: 'False
fallback = Refl
localFlag :: LocalFlag 'x' :~: 'True
localFlag = Refl
localUpper :: LocalUpper 'x' :~: 'X'
localUpper = Refl
