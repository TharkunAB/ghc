{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenDropWhileSpanClient (closedSpanOk, deciderUnambiguous, shapeProofOk)
import qualified NativeOpenSpanShapes as SpanShapes
import PreludeDropWhileProof (rejoinPair)
import PreludeFilterAppendProof (FilterDecisions(..))
import PreludeFilterCutoff (CutoffBound(..), packCutoff, showCutoffBound)
import PreludeFilterLengthProof (AtMost(..))
import PreludeFilterRuntime (BooleanResult(..))
import PreludeFilterSource (Negation)
import PreludeMapLengthSize (Nat(..), Size)
import PreludeSpanProof (rejoinSpan)
import PreludeTakeWhile (PrefixStep(..), RuntimePrefix(..), observePrefix)

passed :: Bool
passed = case (rejoinPair decisions, rejoinSpan decisions) of
  (Refl, Refl) -> closedSpanOk && shapeProofOk && deciderUnambiguous == 0
  where
    decisions :: FilterDecisions Negation '[ 'False, 'True, 'False]
    decisions = Decide IsTrue (Decide IsFalse (error "proof inspected after rejection"))

firstCutoff :: CutoffBound -> String
firstCutoff (CutoffBound _ _ bound) = case bound of
  ZeroBound -> "empty"
  BothSucc _ -> "keep"
  RightSucc _ -> "skip"

sizeFromInt :: Int -> Size
sizeFromInt n
  | n <= 0 = Zero
  | otherwise = Succ (sizeFromInt (n - 1))

firstPrefix :: RuntimePrefix -> String
firstPrefix (RuntimePrefix gate x xs step) = case step of
  Stops Refl -> case takeWhile (gate &&) (x : xs) of
    [] -> "stop"
    _ -> "unexpected cons"
  Keeps Refl -> case takeWhile (gate &&) (x : xs) of
    True : _ -> "keep"
    _ -> "unexpected result"

main :: IO ()
main = do
  let input = False : True : error "rejection forced the suffix tail"
      suffix = dropWhile not input
      rejected = dropWhile (const False) (undefined : undefined :: [Bool])
      needsTail = dropWhile (const True) (undefined : error "accepted tail" :: [Bool])
  demanded <- try (evaluate needsTail)
  print ( passed, takeWhile not input, take 1 suffix, hasCons rejected
        , null (dropWhile (error "empty input forced callback") ([] :: [Bool]))
        , null (dropWhile (const True) [undefined :: Bool])
        , case demanded of Left (ErrorCall "accepted tail") -> True; _ -> False )
  strict <- try (evaluate (firstAccepted SpanShapes.strictSpan))
  spine <- try (evaluate (span id (error "span spine" :: [Bool])))
  predicate <- try (evaluate (span (error "span predicate") [True]))
  let (prefix, rest) = span not input
      finite = [False, True, False]
      (before, after) = span not finite
      unknown = error "separate prefix and suffix spine" :: [Bool]
  print ( firstAccepted span, firstAccepted SpanShapes.lazySpan
        , take 2 (prefix ++ rest), before ++ after == finite
        , raised "span tail" strict, raised "span spine" spine
        , raised "span predicate" predicate
        , (takeWhile id unknown, dropWhile id unknown) `seq` True )
  print ( take 1 (fst (span id (True : undefined)))
        , take 1 (fst (SpanShapes.lazySpan id (True : undefined))) )
  rows <- newIORef [[], [False], [True, False]] >>= readIORef
  mapM_ (\n -> do
    cutoff <- newIORef (sizeFromInt n) >>= readIORef
    putStrLn (showCutoffBound (packCutoff cutoff rows))) [0, 1, 2]
  putStrLn (firstCutoff (packCutoff (error "empty input forced cutoff") []))
  putStrLn (firstCutoff (packCutoff (error "empty row forced cutoff") ([] : error "outer tail")))
  putStrLn (firstCutoff (packCutoff Zero ((error "element" : error "inner tail") : error "outer tail")))
  putStrLn (firstCutoff (packCutoff (Succ Zero) ([error "element"] : error "outer tail")))
  putStrLn (showCutoffBound (packCutoff (Succ Zero) [repeat False]))
  cutoffDemand <- try (evaluate (firstCutoff (packCutoff (error "nonempty row needs cutoff") [[False]])))
  print (case cutoffDemand of Left (ErrorCall "nonempty row needs cutoff") -> True; _ -> False)
  off <- newIORef False >>= readIORef
  on <- newIORef True >>= readIORef
  print ( firstPrefix (observePrefix off undefined undefined)
        , firstPrefix (observePrefix on False undefined)
        , firstPrefix (observePrefix on True undefined) )
  where
    hasCons (_ : _) = True
    hasCons [] = False

    firstAccepted split = case fst (split id (True : error "span tail")) of
      True : _ -> True
      _ -> False

    raised message (Left (ErrorCall actual)) = actual == message
    raised _ _ = False
