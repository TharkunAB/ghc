{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativePolyNestedDomainOpenEqReverse
  ( proofAssoc
  , proofDirectTuple
  , proofUnit
  , proofSum
  ) where

import Data.Type.Equality ((:~:) (Refl))

proofAssoc :: foreach (p :: ((a, b), c)) -> assocProdFrom (assocProdTo p) :~: p
proofAssoc (ab, _) = case ab of
  (_, _) -> Refl

proofDirectTuple :: foreach (p :: ((a, b), c)) -> directTupleFrom (directTupleTo p) :~: p
proofDirectTuple (ab, _) = case ab of
  (_, _) -> Refl

proofUnit :: foreach (x :: a) -> unitRTo (unitRFrom x) :~: x
proofUnit _ = Refl

proofSum :: foreach (s :: Either (Either a b) c) -> sumAssocFrom (sumAssocTo s) :~: s
proofSum (Left ab) = case ab of
  Left _  -> Refl
  Right _ -> Refl
proofSum (Right _) = Refl

directTupleTo :: ((a, b), c) -> (a, (b, c))
directTupleTo ((x, y), z) = (x, (y, z))

directTupleFrom :: (a, (b, c)) -> ((a, b), c)
directTupleFrom (x, (y, z)) = ((x, y), z)

assocProdTo :: ((a, b), c) -> (a, (b, c))
assocProdTo (p, z) = assocPairR p z

assocProdFrom :: (a, (b, c)) -> ((a, b), c)
assocProdFrom (x, p) = assocPairL p x

assocPairR :: (a, b) -> c -> (a, (b, c))
assocPairR (x, y) z = (x, (y, z))

assocPairL :: (b, c) -> a -> ((a, b), c)
assocPairL (y, z) x = ((x, y), z)

unitRTo :: (a, ()) -> a
unitRTo (x, ()) = x

unitRFrom :: a -> (a, ())
unitRFrom x = (x, ())

sumAssocTo :: Either (Either a b) c -> Either a (Either b c)
sumAssocTo (Left (Left x))  = Left x
sumAssocTo (Left (Right y)) = Right (Left y)
sumAssocTo (Right z)        = Right (Right z)

sumAssocFrom :: Either a (Either b c) -> Either (Either a b) c
sumAssocFrom (Left x)          = Left (Left x)
sumAssocFrom (Right (Left y))  = Left (Right y)
sumAssocFrom (Right (Right z)) = Right z
