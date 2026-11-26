{-# LANGUAGE GADTs, NoDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import qualified GHC.Internal.List as List
import ListFoldrProof (showListFoldAppend)
import NativeOpenListFoldrConcrete (showConcreteFold)
import NativeOpenListFoldrExtra (userFoldOk)
import qualified NativeOpenListFoldrProducer as F
import Prelude
import qualified Prelude as P
import qualified PreludeFoldrProof as PFP

{-# NOINLINE demand #-}
demand :: ((Char -> Char -> Char) -> Char -> [Char] -> Char) -> IO ()
demand fold = do
  print (fold (error "empty fold applied combiner") 's' [])
  print (fold (\_ _ -> 'n') (error "forced seed") (error "forced head" : error "forced tail"))
  print (fold (\x _ -> x) (error "forced seed") ('h' : error "forced tail"))
  print (fold (\x _ -> x) (error "forced seed") (repeat 'i'))
  crashed <- try @ErrorCall (evaluate (fold (\_ _ -> 'n') 's' (error "input spine")))
  case crashed of
    Left exception | "input spine" `isInfixOf` displayException exception -> pure ()
    _ -> fail "fold returned before demanding the input spine"

main :: IO ()
main = do
  unless userFoldOk (fail "userFoldOk failed")
  mapM_ demand [List.foldr, P.foldr, F.direct, F.local]
  putStrLn showConcreteFold
  print PFP.example
  putStrLn showListFoldAppend
  putStrLn PFP.showFoldMap
