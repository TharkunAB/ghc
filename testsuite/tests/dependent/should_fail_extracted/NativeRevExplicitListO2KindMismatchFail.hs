{-# LANGUAGE DataKinds, Modifiers, NoDependentTypes, PolyKinds, RankNTypes #-}
module NativeRevExplicitListO2KindMismatchFail where

import Data.Proxy (Proxy)
import GHC.Matchability (Matchability)
import NativeRevExplicitListO2BoxMod (BoxM, RKindU)

matchMismatch :: forall (r :: RKindU). BoxM r -> ()
matchMismatch _ = ()

plainMismatch :: Proxy (Int -> Bool) -> Proxy (Char -> Bool)
plainMismatch x = x

alphaPolyMismatch :: (forall (m :: Matchability) (f :: Int %m -> Bool). Proxy f)
                  -> (forall (n :: Matchability) (h :: Char %n -> Bool). Proxy h)
alphaPolyMismatch x = x
