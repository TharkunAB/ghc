{-# LANGUAGE AllowAmbiguousTypes, DataKinds, NoDependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Proxy (Proxy (..))
import GHC.Sigma (Sigma (Pair), first)
import NativeClosedFamilySumChain (OpenPackage, Outer)
import NativeClosedFamilySumProvider (DirectOpen, KindCapture, KindOpen, Pack, PackViaAlias, Tagged (..))

closed :: Pack False Int
closed = Pair False (FalseTag True)

captured :: Pack True Char
captured = Pair True (TrueTag 'x')

lazyPayload :: Pack True Char
lazyPayload = Pair False (error "unused payload" :: Tagged False Char)

kindCapture :: KindCapture True Bool False
kindCapture = Pair True (Proxy :: Proxy True, Proxy :: Proxy False)

size :: Pack True Char -> Bool
size (Pair tag _) = tag

sizeViaAlias :: PackViaAlias True Char -> Bool
sizeViaAlias (Pair tag _) = tag

sizeKind :: KindCapture True Bool False -> Bool
sizeKind (Pair tag ~(_, _)) = tag

sizeOuter :: Outer True Char -> Bool
sizeOuter (Pair tag _) = tag

sizeOpen :: OpenPackage Bool -> Bool
sizeOpen (Pair tag _) = tag

sizeDirect :: DirectOpen [Int] -> Bool
sizeDirect (Pair tag _) = tag

sizeDirectOther :: DirectOpen Char -> Bool
sizeDirectOther (Pair tag _) = tag

sizeKindOpen :: KindOpen Bool False -> Bool
sizeKindOpen (Pair tag _) = tag

main :: IO ()
main = do
  outer <- try (evaluate (sizeOuter (error "outer package"))) :: IO (Either ErrorCall Bool)
  open <- try (evaluate (sizeOpen (error "open package"))) :: IO (Either ErrorCall Bool)
  case (outer, open) of
    (Left _, Left _) ->
      print (first closed, size captured, size lazyPayload, first kindCapture,
             sizeViaAlias (Pair False (error "alias payload")),
             sizeKind (Pair True (error "lazy tuple payload")),
             sizeOuter (Pair False (error "chained payload")),
             sizeOpen (Pair True (error "open payload")),
             sizeDirect (Pair True (error "captured open payload")),
             sizeDirectOther (Pair False (FalseTag True)),
             sizeKindOpen (Pair True (Proxy :: Proxy True, Proxy :: Proxy False)))
    _ -> fail "package pattern left the outer constructor unevaluated"
