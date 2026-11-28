{-# LANGUAGE DependentTypes #-}
module Main (main) where

import Control.Exception (ErrorCall (..), evaluate, throwIO, try)
import Data.Type.Equality ((:~:) (Refl))
import Data.Void (Void, absurd)
import GHC.Tuple (Tuple2, Unit)

import NativeVoidAbsurdOpenEqProvider

-- Same-module empty-case and absurd functions for staging coverage.
localSumZeroRTo :: Either a Void -> a
localSumZeroRTo (Left x)  = x
localSumZeroRTo (Right v) = case v of {}

localAbsurdLTo :: Either Void a -> a
localAbsurdLTo (Left v)  = absurd v
localAbsurdLTo (Right x) = x

localRLR :: foreach (s :: Either a Void) -> Left (localSumZeroRTo s) :~: s
localRLR (Left _)  = Refl
localRLR (Right v) = case v of {}

localRRL :: foreach (x :: a) -> localSumZeroRTo (Left x) :~: x
localRRL _ = Refl

localLLR :: foreach (s :: Either Void a) -> Right (localAbsurdLTo s) :~: s
localLLR (Left v)  = case v of {}
localLLR (Right _) = Refl

localLRL :: foreach (x :: a) -> localAbsurdLTo (Right x) :~: x
localLRL _ = Refl

-- Cross-module isomorphism proofs over imported source tables.
sumZeroRLR :: foreach (s :: Either a Void) -> sumZeroRFrom (sumZeroRTo s) :~: s
sumZeroRLR (Left _)  = Refl
sumZeroRLR (Right v) = case v of {}

sumZeroRRL :: foreach (x :: a) -> sumZeroRTo (sumZeroRFrom x) :~: x
sumZeroRRL _ = Refl

sumZeroLLR :: foreach (s :: Either Void a) -> sumZeroLFrom (sumZeroLTo s) :~: s
sumZeroLLR (Left v)  = case v of {}
sumZeroLLR (Right _) = Refl

sumZeroLRL :: foreach (x :: a) -> sumZeroLTo (sumZeroLFrom x) :~: x
sumZeroLRL _ = Refl

absurdRLR :: foreach (s :: Either a Void) -> Left (absurdRTo s) :~: s
absurdRLR (Left _)  = Refl
absurdRLR (Right v) = absurd v

absurdRRL :: foreach (x :: a) -> absurdRTo (Left x) :~: x
absurdRRL _ = Refl

emptyLeftLR :: foreach (s :: Either Empty a) -> Right (emptyLeftTo s) :~: s
emptyLeftLR (Left e)  = case e of {}
emptyLeftLR (Right _) = Refl

nestedVoidLR :: foreach (p :: Tuple2 (Either a Void) b) -> nestedVoidTo p :~: nestedVoidTo p
nestedVoidLR _ = Refl

nestedVoidEval :: foreach (x :: a) -> foreach (y :: b) -> nestedVoidTo (Left x, y) :~: (x, y)
nestedVoidEval _ _ = Refl

monoVoidEval :: foreach (n :: Int) -> monoVoidTo (Left n) :~: n
monoVoidEval _ = Refl

maybeSumZeroREval :: foreach (x :: a) -> maybeSumZeroR (Just (Left x)) :~: Just x
maybeSumZeroREval _ = Refl

closedVoidEval :: sumZeroRTo (Left True) :~: True
closedVoidEval = Refl

checkIso :: (Eq a, Eq b) => Iso a b -> a -> b -> IO Unit
checkIso (MkIso f g fg gf) x y =
  case (fg x, gf y) of
    (Refl, Refl) ->
      if f x == y && g y == x
        then pure ()
        else throwIO (ErrorCall "Iso runtime mismatch")

runProofs
  :: Either Int Void
  -> Either Void String
  -> Either Char Void
  -> Either Empty Bool
  -> Tuple2 (Either Int Void) Bool
  -> Int
  -> String
  -> IO Unit
runProofs sR sL sChar sEmpty pairVal n s =
  case ( localRLR sR
       , localRRL s
       , localLLR sL
       , localLRL 'z'
       , sumZeroRLR sR
       , sumZeroRRL False
       , sumZeroLLR sL
       , sumZeroLRL n
       , absurdRLR sChar
       , absurdRRL n
       , emptyLeftLR sEmpty
       , nestedVoidLR pairVal
       , nestedVoidEval n False
       , monoVoidEval n
       , maybeSumZeroREval True
       , closedVoidEval
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl) ->
      pure ()

main :: IO Unit
main = do
  checkIso sumZeroRIso (Left (42 :: Int)) 42
  checkIso sumZeroLIso (Right "ok") "ok"
  runProofs (Left 42) (Right "hi") (Left 'a') (Right True) (Left 3, False) 12 "ok"
  res <- try (evaluate (sumZeroRTo (Right (error "strict-void") :: Either Int Void)))
  case res of
    Left (ErrorCall msg) | msg == "strict-void" -> pure ()
    other -> throwIO (ErrorCall ("unexpected result: " ++ show other))
