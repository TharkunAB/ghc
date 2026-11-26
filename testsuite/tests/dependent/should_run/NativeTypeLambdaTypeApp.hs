{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import NativeTypeLambdaTypeAppCalls
  ( captured
  , ordinarySplice
  , ordinaryWildcard
  , shadowed
  , spliced
  , splicedMulti
  )
import TypeLambda (Fields(..))
import TypeLambdaArgument (messageCells)

printCells :: Fields '[Either String Int, Either String Bool] -> IO ()
printCells (Field count (Field flag End)) = print (count, flag)

cells :: Fields '[Either e Int, Either e Bool] -> (Either e Int, Either e Bool)
cells (Field count (Field flag End)) = (count, flag)

main :: IO ()
main = do
  printCells (messageCells "missing flag")
  printCells (shadowed (Proxy @Ordering) "shadowed flag")
  printCells spliced
  printCells (splicedMulti "type-spliced flag")
  print ordinaryWildcard
  print ordinarySplice
  print (cells (captured (Proxy @Bool) (Proxy @Ordering)))
  print (cells (captured (Proxy @Char) (Proxy @())))
