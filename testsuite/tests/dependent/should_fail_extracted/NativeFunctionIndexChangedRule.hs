{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeFunctionIndexChangedRule where
import Data.Type.Equality ((:~:)(Refl))
import NativeFunctionIndexIsoIdentity (identity)

{-# RULES "change identity at False" identity False = True #-}

roundTrip :: foreach (x :: a) -> identity (identity x) :~: x
roundTrip _ = Refl
