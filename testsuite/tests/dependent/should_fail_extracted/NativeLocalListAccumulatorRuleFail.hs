{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeLocalListAccumulatorRuleFail where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeLocalListAccumulatorRevSource as RS

shunt :: [a] -> [a] -> [a]
shunt [] ys = ys
shunt (x : xs) ys = shunt xs (x : ys)

{-# RULES "skip shunt" forall xs ys. shunt xs ys = ys #-}

step :: foreach (x :: a) -> foreach (xs :: [a]) -> foreach (ys :: [a])
     -> shunt (x ': xs) ys :~: shunt xs (x ': ys)
step _ _ _ = Refl

{-# RULES "skip append" forall xs. RS.app [] xs = [] #-}

importedStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
             -> RS.rev (x ': xs) :~: RS.app (RS.rev xs) '[x]
importedStep _ _ = Refl
