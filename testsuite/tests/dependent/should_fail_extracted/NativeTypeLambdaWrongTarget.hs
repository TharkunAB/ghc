{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module NativeTypeLambdaWrongTarget where
import Data.Kind (Type)
import GHC.Exts (U)
type LeibnizU (x :: a) (y :: a) = forall (p :: a %U -> Type). p x -> p y
leibnizUSymAt :: forall {a} (x :: a) (y :: a) (p :: a %U -> Type). LeibnizU x y -> p y -> p x
leibnizUSymAt xy = xy @(\z -> p z -> p y) (\px -> px)
