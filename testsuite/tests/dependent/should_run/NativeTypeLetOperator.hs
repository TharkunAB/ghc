{-# LANGUAGE DependentTypes, ListTuplePuns, LinearTypes, StarIsType, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-star-is-type #-}
module NativeTypeLetOperator where
import GHC.Exts (Multiplicity)
type Result (f :: * -> * -> *) a b flag =
    let field = a `f` b in case flag of { True -> Maybe field; False -> [field] }
type Prefix f a b flag =
    let field = f a b in case flag of { True -> Maybe field; False -> [field] }
type Handler (m :: Multiplicity) a b flag =
    let callback = a %m -> b in case flag of { True -> Maybe callback; False -> [callback] }
