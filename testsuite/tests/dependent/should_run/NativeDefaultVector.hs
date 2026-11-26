{-# LANGUAGE NoDependentTypes, TypeApplications #-}
module Main where
import DefaultVector
import PackedVecLibrary

instance DefaultElement Char where
  defaultElement = 'x'

main :: IO ()
main = print (toList (filled @Char 3))
