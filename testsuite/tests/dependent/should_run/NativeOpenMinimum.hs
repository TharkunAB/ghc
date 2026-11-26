{-# LANGUAGE GADTs, NoDependentTypes #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenMinimumDormantRule ()
import qualified PreludeZipRuntime as PZR
import qualified ZipMinimum as ZM

checkPZR :: [Bool] -> [()] -> Bool
checkPZR xs ys = case PZR.lengthZip xs ys of Refl -> True

checkZM :: [Bool] -> [()] -> Bool
checkZM xs ys = case ZM.lengthZip xs ys of Refl -> True

main :: IO ()
main = do
  print (checkPZR [True, False] [()], checkPZR [] (error "forced right list"))
  print (checkZM [True, False] [()], checkZM [] (error "forced right list"))
