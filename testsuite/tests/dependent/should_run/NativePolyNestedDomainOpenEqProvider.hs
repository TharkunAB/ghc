{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativePolyNestedDomainOpenEqProvider
  ( Iso (..)
  , Pair (..)
  , assocProdTo
  , assocProdFrom
  , assocDirectTo
  , assocDirectFrom
  , unitRTo
  , unitRToLazy
  , unitRFrom
  , unitLTo
  , unitLFrom
  , sumAssocTo
  , sumAssocFrom
  , distribRTo
  , distribRFrom
  , distribLTo
  , distribLFrom
  , unconsPair
  , reconsPair
  , prodAssocIso
  , prodAssocDirectIso
  , prodUnitRIso
  , prodUnitLIso
  , sumAssocIso
  , prodDistribSumIso
  , prodDistribSumLIso
  , listPairIso
  ) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))

-- | Isomorphism between two types with round-trip witnesses.
data Iso (a :: Type) (b :: Type) where
  MkIso
    :: foreach (f :: a -> b)
    -> foreach (g :: b -> a)
    -> (foreach (x :: a) -> g (f x) :~: x)
    -> (foreach (y :: b) -> f (g y) :~: y)
    -> Iso a b

-- | Reassociate pairs rightward through a private carried-argument helper.
assocProdTo :: ((a, b), c) -> (a, (b, c))
assocProdTo (p, z) = assocPairR p z

-- | Private helper matching a pair with a carried polymorphic parameter.
assocPairR :: (a, b) -> c -> (a, (b, c))
assocPairR (x, y) z = (x, (y, z))

-- | Private helper matching a pair with a carried left parameter.
assocPairL :: (b, c) -> a -> ((a, b), c)
assocPairL (y, z) x = ((x, y), z)

-- | Reassociate pairs leftward through a private carried-argument helper.
assocProdFrom :: (a, (b, c)) -> ((a, b), c)
assocProdFrom (x, p) = assocPairL p x

assocProdRT :: foreach (p :: ((a, b), c)) -> assocProdFrom (assocProdTo p) :~: p
assocProdRT (ab, _) = case ab of
  (_, _) -> Refl

assocProdLF :: foreach (q :: (a, (b, c))) -> assocProdTo (assocProdFrom q) :~: q
assocProdLF (_, bc) = case bc of
  (_, _) -> Refl

-- | Associativity of pairs via private carried-argument helpers.
prodAssocIso :: Iso ((a, b), c) (a, (b, c))
prodAssocIso = MkIso assocProdTo assocProdFrom assocProdRT assocProdLF

-- | Local parameterized pair datatype to test direct nested constructor patterns
-- and same-module early datatype scheduling alongside deriving.
data Pair a b = MkPair a b
  deriving (Eq, Show)

-- | Direct nested constructor pattern associativity.
assocDirectTo :: Pair (Pair a b) c -> Pair a (Pair b c)
assocDirectTo (MkPair (MkPair x y) z) = MkPair x (MkPair y z)

-- | Inverse direct nested constructor pattern associativity.
assocDirectFrom :: Pair a (Pair b c) -> Pair (Pair a b) c
assocDirectFrom (MkPair x (MkPair y z)) = MkPair (MkPair x y) z

assocDirectRT :: foreach (p :: Pair (Pair a b) c) -> assocDirectFrom (assocDirectTo p) :~: p
assocDirectRT (MkPair ab _) = case ab of
  MkPair _ _ -> Refl

assocDirectLF :: foreach (q :: Pair a (Pair b c)) -> assocDirectTo (assocDirectFrom q) :~: q
assocDirectLF (MkPair _ bc) = case bc of
  MkPair _ _ -> Refl

-- | Associativity of Pair via direct nested patterns.
prodAssocDirectIso :: Iso (Pair (Pair a b) c) (Pair a (Pair b c))
prodAssocDirectIso = MkIso assocDirectTo assocDirectFrom assocDirectRT assocDirectLF

-- | Right unit elimination matching the inner unit constructor.
unitRTo :: (a, ()) -> a
unitRTo (x, ()) = x

-- | Right unit elimination with a wildcard on the unit component.
unitRToLazy :: (a, ()) -> a
unitRToLazy (x, _) = x

-- | Right unit introduction as a zero-match constructor producer.
unitRFrom :: a -> (a, ())
unitRFrom x = (x, ())

unitRRT :: foreach (p :: (a, ())) -> unitRFrom (unitRTo p) :~: p
unitRRT (_, u) = case u of
  () -> Refl

unitRLF :: foreach (x :: a) -> unitRTo (unitRFrom x) :~: x
unitRLF _ = Refl

-- | Right identity of product.
prodUnitRIso :: Iso (a, ()) a
prodUnitRIso = MkIso unitRTo unitRFrom unitRRT unitRLF

-- | Left unit elimination matching the inner unit constructor.
unitLTo :: ((), a) -> a
unitLTo ((), x) = x

-- | Left unit introduction as a zero-match constructor producer.
unitLFrom :: a -> ((), a)
unitLFrom x = ((), x)

unitLRT :: foreach (p :: ((), a)) -> unitLFrom (unitLTo p) :~: p
unitLRT (u, _) = case u of
  () -> Refl

unitLLF :: foreach (x :: a) -> unitLTo (unitLFrom x) :~: x
unitLLF _ = Refl

-- | Left identity of product.
prodUnitLIso :: Iso ((), a) a
prodUnitLIso = MkIso unitLTo unitLFrom unitLRT unitLLF

-- | Associativity of sums forward.
sumAssocTo :: Either (Either a b) c -> Either a (Either b c)
sumAssocTo (Left (Left x))  = Left x
sumAssocTo (Left (Right y)) = Right (Left y)
sumAssocTo (Right z)        = Right (Right z)

-- | Associativity of sums backward.
sumAssocFrom :: Either a (Either b c) -> Either (Either a b) c
sumAssocFrom (Left x)          = Left (Left x)
sumAssocFrom (Right (Left y))  = Left (Right y)
sumAssocFrom (Right (Right z)) = Right z

sumAssocRT :: foreach (s :: Either (Either a b) c) -> sumAssocFrom (sumAssocTo s) :~: s
sumAssocRT (Left ab) = case ab of
  Left _  -> Refl
  Right _ -> Refl
sumAssocRT (Right _) = Refl

sumAssocLF :: foreach (t :: Either a (Either b c)) -> sumAssocTo (sumAssocFrom t) :~: t
sumAssocLF (Left _)   = Refl
sumAssocLF (Right bc) = case bc of
  Left _  -> Refl
  Right _ -> Refl

-- | Associativity of disjunction.
sumAssocIso :: Iso (Either (Either a b) c) (Either a (Either b c))
sumAssocIso = MkIso sumAssocTo sumAssocFrom sumAssocRT sumAssocLF

-- | Right distributivity of product over sum forward.
distribRTo :: (Either a b, c) -> Either (a, c) (b, c)
distribRTo (Left x, z)  = Left (x, z)
distribRTo (Right y, z) = Right (y, z)

-- | Right distributivity of product over sum backward.
distribRFrom :: Either (a, c) (b, c) -> (Either a b, c)
distribRFrom (Left (x, z))  = (Left x, z)
distribRFrom (Right (y, z)) = (Right y, z)

distribRRT :: foreach (p :: (Either a b, c)) -> distribRFrom (distribRTo p) :~: p
distribRRT (ab, _) = case ab of
  Left _  -> Refl
  Right _ -> Refl

distribRLF :: foreach (s :: Either (a, c) (b, c)) -> distribRTo (distribRFrom s) :~: s
distribRLF (Left ac) = case ac of
  (_, _) -> Refl
distribRLF (Right bc) = case bc of
  (_, _) -> Refl

-- | Right distributivity isomorphism.
prodDistribSumIso :: Iso (Either a b, c) (Either (a, c) (b, c))
prodDistribSumIso = MkIso distribRTo distribRFrom distribRRT distribRLF

-- | Left distributivity of product over sum forward.
distribLTo :: (a, Either b c) -> Either (a, b) (a, c)
distribLTo (x, Left y)  = Left (x, y)
distribLTo (x, Right z) = Right (x, z)

-- | Left distributivity of product over sum backward.
distribLFrom :: Either (a, b) (a, c) -> (a, Either b c)
distribLFrom (Left (x, y))  = (x, Left y)
distribLFrom (Right (x, z)) = (x, Right z)

distribLRT :: foreach (p :: (a, Either b c)) -> distribLFrom (distribLTo p) :~: p
distribLRT (_, bc) = case bc of
  Left _  -> Refl
  Right _ -> Refl

distribLLF :: foreach (s :: Either (a, b) (a, c)) -> distribLTo (distribLFrom s) :~: s
distribLLF (Left ab) = case ab of
  (_, _) -> Refl
distribLLF (Right ac) = case ac of
  (_, _) -> Refl

-- | Left distributivity isomorphism.
prodDistribSumLIso :: Iso (a, Either b c) (Either (a, b) (a, c))
prodDistribSumLIso = MkIso distribLTo distribLFrom distribLRT distribLLF

-- | Split an inductive list nested inside a pair.
unconsPair :: ([a], b) -> Either b (a, ([a], b))
unconsPair ([], y)     = Left y
unconsPair (x : xs, y) = Right (x, (xs, y))

-- | Rebuild an inductive list nested inside a pair from a three-level sum/pair pattern.
reconsPair :: Either b (a, ([a], b)) -> ([a], b)
reconsPair (Left y)            = ([], y)
reconsPair (Right (x, (xs, y))) = (x : xs, y)

listPairRT :: foreach (p :: ([a], b)) -> reconsPair (unconsPair p) :~: p
listPairRT (xs, _) = case xs of
  []    -> Refl
  _ : _ -> Refl

listPairLF :: foreach (s :: Either b (a, ([a], b))) -> unconsPair (reconsPair s) :~: s
listPairLF (Left _)  = Refl
listPairLF (Right p) = case p of
  (_, q) -> case q of
    (_, _) -> Refl

-- | Inductive list uncons isomorphism inside a product.
listPairIso :: Iso ([a], b) (Either b (a, ([a], b)))
listPairIso = MkIso unconsPair reconsPair listPairRT listPairLF
