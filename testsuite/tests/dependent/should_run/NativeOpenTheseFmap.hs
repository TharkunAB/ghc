{-# LANGUAGE NoDependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenTheseFmapUniform (URow(..), thatProof, thisProof, uBoth, uEmpty, uFlagged)
import OrdinaryTheseMapping (report, reportAt)
import Row (Row(..))
import qualified RowProof
import These (These(..))
import TheseMapping (mapAt, mapBoth)

printProof :: (a -> Bool) -> Char -> a -> IO ()
printProof p label x = print (mapBoth p label x)

forceProof :: (a -> b) -> Char -> a -> IO ()
forceProof f label x = evaluate (mapBoth f label x) >> pure ()

keepAt :: (a -> b -> c) -> a -> Char -> b -> IO ()
keepAt f pivot tag x = evaluate (mapAt f pivot tag x) >> pure ()

rowProofs :: (Int -> Bool) -> Int -> Bool -> Int -> IO ()
rowProofs p label flag x = do
  print (fmap p (Flagged False 11 :: Row Bool Bool Int), RowProof.discarded p flag x)
  print (RowProof.both p label flag x)

main :: IO ()
main = do
  putStrLn (report (> (4 :: Int)) 'w' 5)
  putStrLn (report (replicate 4) 'w' 'a')
  putStrLn (report fromEnum 'w' True)
  putStrLn (reportAt replicate (3 :: Int) True 'v')
  putStrLn (reportAt (flip compare) False 'w' True)
  keepAt (error "forced callback" :: Int -> Bool -> Ordering)
         (error "forced pivot") (error "forced tag") (error "forced element")
  printProof (error "predicate") (error "label") (error "value" :: Int)
  ref <- newIORef (0 :: Int)
  let action _ = modifyIORef' ref (+ 1) >> readIORef ref
      above n = (> n) :: Int -> Bool
  keepAt (\n _ -> modifyIORef' ref (+ n)) (3 :: Int) 'i' True
  forceProof action 'i' True
  print =<< readIORef ref
  case fmap action (These 'i' True) of
    These label io -> do n <- io; print (label, n)
    _ -> fail "fmap discarded the action"
  forceProof above 'f' (4 :: Int)
  case fmap above (These 'f' (4 :: Int)) of
    These label p -> print (label, p 3, p 5)
    _ -> fail "fmap discarded the function"
  let row = Both (error "first field") (error "second field") (11 :: Int) :: Row Int Bool Int
  case fmap odd row of
    Both _ _ x -> print x
    _ -> fail "fmap discarded the row"
  rowProofs odd (error "first field") (error "second field") (error "element")
  (thisProof not 'a', thatProof not True) `seq` pure ()
  let discard :: URow Bool Bool Int -> URow Bool Bool Bool
      discard = fmap (error "forced callback")
      cell = error "forced cell"
  case discard (UBoth cell cell cell) of
    UEmpty -> pure ()
    _ -> fail "expected UEmpty"
  result <- try (evaluate (discard (error "forced receiver")))
            :: IO (Either ErrorCall (URow Bool Bool Bool))
  print (either (isInfixOf "forced receiver" . displayException) (const False) result)
