{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeLocalListAccumulatorRevProof (revStep) where

import Data.Type.Equality ((:~:)(Refl))
import NativeLocalListAccumulatorRevSource (app, rev)

revStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
        -> rev (x ': xs) :~: app (rev xs) '[x]
revStep _ _ = Refl
