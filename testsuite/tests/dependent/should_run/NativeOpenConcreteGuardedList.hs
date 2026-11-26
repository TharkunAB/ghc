{-# LANGUAGE NoDependentTypes #-}
module Main where

import ConcreteGuardedList (dropBool, dropMarkers)
import ConcreteGuardedListProof (showConcreteGuardedLists)

main :: IO ()
main = do
  putStrLn showConcreteGuardedLists
  print ( take 1 (dropBool (const True) (undefined : True : undefined))
        , hasCons (dropMarkers (const False) (undefined : undefined)) )
  where
    hasCons (_ : _) = True
    hasCons [] = False
