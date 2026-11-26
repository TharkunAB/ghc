{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoDependentTypes #-}
module NativeClosedFamilySumClientRejects where

import GHC.Sigma (Sigma (Pair))
import NativeClosedFamilySumChain (Loop, OpenLoop, OpenPackage, Outer)
import NativeClosedFamilySumProvider (Pack, Tagged (..))

badTag :: Pack True Char
badTag = Pair False (TrueTag 'x')

lazyPackage :: Pack True Char -> Bool
lazyPackage ~(Pair tag _) = tag

stuckPackage :: Outer select Char -> Bool
stuckPackage (Pair tag _) = tag

newtype WrappedPackage = WrappedPackage (Pack True Char)

wrappedPackage :: WrappedPackage -> Bool
wrappedPackage (Pair tag _) = tag

loopPackage :: Loop Char -> Bool
loopPackage (Pair tag _) = tag

stuckOpen :: OpenPackage Char -> Bool
stuckOpen (Pair tag _) = tag

wrappedOpen :: OpenPackage Int -> Bool
wrappedOpen (Pair tag _) = tag

loopOpen :: OpenLoop Char -> Bool
loopOpen (Pair tag _) = tag
