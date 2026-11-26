{-# LANGUAGE DataKinds, NoDependentTypes, TypeApplications, TypeOperators #-}
module Main where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenBuildExtra ()
import OpenBuildSource (Make, Swap, exact)
import qualified OpenBuildWorker as B

proof :: Make Bool Char :~: 'B.Shell Bool Char
proof = exact (Proxy @Bool) (Proxy @Char)

late :: Swap '( 'True, 'x') :~: '( 'x', 'True)
late = Refl

main :: IO ()
main = case (proof, late) of
  (Refl, Refl) -> putStrLn "constructed open result"
