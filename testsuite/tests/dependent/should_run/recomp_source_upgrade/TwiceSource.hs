{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module TwiceSource (Twice) where
import qualified NativeOpenInstalledMapProducer as W
import qualified Prelude as P

type Twice :: [P.Bool] -> [P.Bool]
type Twice xs = W.mapper P.not (W.mapper P.not xs)
