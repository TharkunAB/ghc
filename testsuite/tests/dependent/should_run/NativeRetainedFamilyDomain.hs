{-# LANGUAGE NoDependentTypes #-}
module Main (main) where

import ConstrainedElementsClient (retainedMaybeDomain)
import ConstrainedFormatClient (renderedRawView)
import NativeRetainedFamilyDomainChecks (ok, prefix, forgotten)

main :: IO ()
main = do
    ok `seq` prefix `seq` forgotten True `seq` pure ()
    _ <- retainedMaybeDomain
    putStrLn renderedRawView
