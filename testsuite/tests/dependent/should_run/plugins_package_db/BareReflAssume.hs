{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fplugin=Dependent.Plugin.ObsEq #-}
module Main (main) where
import Data.Type.Equality ((:~:) (Refl))
import ObsProvider (Nat(..), plus, stepUnfoldAdd)

bareLaw :: foreach (m :: Nat) (n :: Nat) -> stepUnfoldAdd m n :~: plus m n
bareLaw m n = Refl

main :: IO ()
main = case bareLaw (Succ Zero) (Succ Zero) of Refl -> pure ()
