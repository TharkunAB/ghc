{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Main (main) where

import Control.Exception (ErrorCall (..), throwIO)
import Data.Type.Equality ((:~:) (Refl))
import NativePolyNestedDomainOpenEqProvider
import NativePolyNestedDomainOpenEqReverse (proofAssoc, proofDirectTuple, proofSum, proofUnit)

clientAssocRT :: foreach (p :: ((a, b), c)) -> assocProdFrom (assocProdTo p) :~: p
clientAssocRT (ab, _) = case ab of
  (_, _) -> Refl

clientAssocDirectRT :: foreach (p :: Pair (Pair a b) c) -> assocDirectFrom (assocDirectTo p) :~: p
clientAssocDirectRT (MkPair ab _) = case ab of
  MkPair _ _ -> Refl

clientUnitR :: foreach (x :: a) -> unitRTo (unitRFrom x) :~: x
clientUnitR _ = Refl

clientUnitRLazy :: foreach (x :: a) -> unitRToLazy (unitRFrom x) :~: x
clientUnitRLazy _ = Refl

clientUnitL :: foreach (x :: a) -> unitLTo (unitLFrom x) :~: x
clientUnitL _ = Refl

clientSumAssocRT :: foreach (s :: Either (Either a b) c) -> sumAssocFrom (sumAssocTo s) :~: s
clientSumAssocRT (Left ab) = case ab of
  Left _  -> Refl
  Right _ -> Refl
clientSumAssocRT (Right _) = Refl

clientDistribRT :: foreach (p :: (Either a b, c)) -> distribRFrom (distribRTo p) :~: p
clientDistribRT (ab, _) = case ab of
  Left _  -> Refl
  Right _ -> Refl

clientDistribLRT :: foreach (p :: (a, Either b c)) -> distribLFrom (distribLTo p) :~: p
clientDistribLRT (_, bc) = case bc of
  Left _  -> Refl
  Right _ -> Refl

clientListPairRT :: foreach (p :: ([a], b)) -> reconsPair (unconsPair p) :~: p
clientListPairRT (xs, _) = case xs of
  []    -> Refl
  _ : _ -> Refl

checkIso :: (Eq a, Eq b) => Iso a b -> a -> b -> IO ()
checkIso (MkIso f g fg gf) x y = do
  case (fg x, gf y) of
    (Refl, Refl) ->
      if f x == y && g y == x
        then pure ()
        else throwIO (ErrorCall "Iso runtime mismatch")

main :: IO ()
main = do
  checkIso prodAssocIso ((1 :: Int, True), 'a') (1, (True, 'a'))
  checkIso prodAssocDirectIso (MkPair (MkPair "k" False) (3 :: Int)) (MkPair "k" (MkPair False 3))
  checkIso prodUnitRIso (42 :: Int, ()) 42
  checkIso prodUnitLIso ((), "ok") "ok"
  checkIso sumAssocIso (Left (Right True) :: Either (Either Int Bool) Char) (Right (Left True))
  checkIso prodDistribSumIso (Right False :: Either Int Bool, 'z') (Right (False, 'z'))
  checkIso prodDistribSumLIso ('q', Left (9 :: Int) :: Either Int Bool) (Left ('q', 9))
  checkIso listPairIso ([True, False], 'w') (Right (True, ([False], 'w')))
  case ( clientAssocRT ((True, False), ())
       , clientAssocDirectRT (MkPair (MkPair True False) ())
       , clientUnitR True
       , clientUnitRLazy False
       , clientUnitL 'x'
       , clientSumAssocRT (Left (Left True) :: Either (Either Bool Int) ())
       , clientDistribRT (Left True :: Either Bool Int, ())
       , clientDistribLRT ((), Right False :: Either Int Bool)
       , clientListPairRT ([True, False], ())
       , proofAssoc ((True, False), ())
       , proofDirectTuple ((True, False), ())
       , proofUnit True
       , proofSum (Right () :: Either (Either Bool Int) ())
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> pure ()
