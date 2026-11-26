{-# LANGUAGE DataKinds, NoDependentTypes #-}
module Main (main) where

import GHC.Sigma (Sigma (Pair), first)
import NativeKnownPairProvider

openValue :: Open Bool
openValue = Pair True (error "unused payload" :: Payload True)

openFlag :: Open Bool -> Bool
openFlag p = first p

openNested :: Bool
openNested = openFlag (Pair True (error "unused nested payload" :: Payload True))

namedValue :: Named
namedValue = Pair True (error "unused payload" :: Payload True)

namedFlag :: Named -> Bool
namedFlag p = first p

namedNested :: Bool
namedNested = namedFlag (Pair True (error "unused nested payload" :: Payload True))

main :: IO ()
main = do
  print (openFlag openValue, openNested)
  print (namedFlag namedValue, namedNested)
