{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeLocalListAccumulatorFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeLocalListAccumulatorLib (rev, shunt)
import qualified NativeLocalListAccumulatorRevSource as RS

badLocalStep :: foreach (x :: a) -> foreach (xs :: [a]) -> foreach (ys :: [a])
             -> shunt (x ': xs) ys :~: shunt xs ys
badLocalStep _ _ _ = Refl

badClientOpen :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
              -> shunt ('True ': xs) ys :~: shunt xs ('False ': ys)
badClientOpen _ _ = Refl

badLocalRev :: foreach (x :: Bool) -> foreach (xs :: [Bool])
            -> rev (x ': xs) :~: (rev xs ++ '[ 'False])
badLocalRev _ _ = Refl

badImportedRev :: foreach (x :: Bool) -> foreach (xs :: [Bool])
               -> RS.rev (x ': xs) :~: RS.app (RS.rev xs) '[ 'False]
badImportedRev _ _ = Refl
