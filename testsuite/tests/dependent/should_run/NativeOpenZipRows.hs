{-# LANGUAGE GADTs, NoDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenZipRowsLaws (choiceNumber, which)
import NativeOpenZipRowsVariants (countEither, countLeft, countRight)
import OrdinaryZipRows (zipReport)
import PackedVecLibrary (natToInt, toList)
import ZipRows (zipMinimum, zipRows)

minimumHolds :: [a] -> [b] -> Bool
minimumHolds xs ys = case zipMinimum xs ys of Refl -> True

main :: IO ()
main = do
  print (toList (zipRows "ab" [True]), toList (zipRows "a" [True, False]))
  let infinite = repeat 'a'
      badFirst = error "first spine" :: String
      badSecond = error "second spine" :: [Bool]
      badChar = error "first element" :: Char
      badBool = error "second element" :: Bool
      emptyB = [] :: [Bool]
      leftTail = 'a' : error "first tail"
      rightTail = True : error "second tail"
  print (toList (zipRows infinite [True, False]))
  print (null (toList (zipRows "" badSecond)))
  print (length (toList (zipRows [badChar] [badBool])))
  print (take 1 (toList (zipRows leftTail rightTail)), null (toList (zipRows leftTail emptyB)))
  first <- try @ErrorCall (evaluate (null (toList (zipRows badFirst emptyB))))
  print (either (isPrefixOf "first spine" . displayException) (const False) first)
  print (choiceNumber (which "" badSecond), choiceNumber (which "a" emptyB), choiceNumber (which "a" [True]))
  second <- try @ErrorCall (evaluate (choiceNumber (which "a" badSecond)))
  print (either (isPrefixOf "second spine" . displayException) (const False) second)
  print (minimumHolds "" badSecond, minimumHolds [badChar] [badBool],
         minimumHolds infinite [True, False])
  print (natToInt (countRight "" badSecond),
         natToInt (countRight (badChar : error "first tail") [True, False]),
         natToInt (countLeft "ab" rightTail), natToInt (countEither [False, True] [True, False]))
  rightFirst <- try @ErrorCall (evaluate (natToInt (countRight badFirst emptyB)))
  print (either (isPrefixOf "first spine" . displayException) (const False) rightFirst)
  putStrLn (zipReport "abc" [True, False])
  putStrLn (zipReport "" badSecond)
