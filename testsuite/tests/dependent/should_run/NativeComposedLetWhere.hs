{-# LANGUAGE AllowAmbiguousTypes, ExperimentalDependentTypes, GADTs, Modifiers, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl), trans)
import GHC.Matchability (Matchability)

data Iso a b where
  MkIso :: foreach (to :: a -> b) -> foreach (from :: b -> a)
        -> (foreach (x :: a) -> from (to x) :~: x)
        -> (foreach (y :: b) -> to (from y) :~: y)
        -> Iso a b

cong :: forall {a} {b} {m :: Matchability} (f :: a %m -> b) x y.
        x :~: y -> f x :~: f y
cong Refl = Refl

-- | Signed where-clauses with 1-arg lazy compositions (R04).
packWhereSigned
  :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
  -> foreach (h :: b -> c) -> foreach (k :: c -> b)
  -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
  -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
  -> Iso a c
packWhereSigned f g h k left right = MkIso @a @c to from left right
  where
    to :: a -> c
    to x = h (f x)
    from :: c -> a
    from y = g (k y)

-- | Unsigned where-clauses with 1-arg lazy compositions.
packWhereUnsigned
  :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
  -> foreach (h :: b -> c) -> foreach (k :: c -> b)
  -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
  -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
  -> Iso a c
packWhereUnsigned f g h k left right = MkIso @a @c to from left right
  where
    to x = h (f x)
    from y = g (k y)

-- | Local let with 1-arg lazy compositions.
packLet
  :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
  -> foreach (h :: b -> c) -> foreach (k :: c -> b)
  -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
  -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
  -> Iso a c
packLet f g h k left right =
  let to :: a -> c
      to x = h (f x)
      from :: c -> a
      from y = g (k y)
  in MkIso @a @c to from left right

-- | 0-arg dot, 0-arg lambda, and eta-expanded composition forms.
packForms
  :: forall a b c. foreach (f :: a -> b) -> foreach (g :: b -> a)
  -> foreach (h :: b -> c) -> foreach (k :: c -> b)
  -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
  -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
  -> (Iso a c, Iso a c)
packForms f g h k left right =
  let to1 = h . f
      from1 = \y -> g (k y)
      to2 x = (h . f) x
      from2 y = g (k y)
  in (MkIso @a @c to1 from1 left right, MkIso @a @c to2 from2 left right)

-- | Chained 3-function composition in where and let.
packChained
  :: forall a b c d.
     foreach (f :: a -> b) -> foreach (g :: b -> a)
  -> foreach (h :: b -> c) -> foreach (k :: c -> b)
  -> foreach (p :: c -> d) -> foreach (q :: d -> c)
  -> (foreach (x :: a) -> g (k (q (p (h (f x))))) :~: x)
  -> (foreach (y :: d) -> p (h (f (g (k (q y))))) :~: y)
  -> (Iso a d, Iso a d)
packChained f g h k p q left right = (iso1, iso2)
  where
    iso1 :: Iso a d
    iso1 = let u = p . h . f
               v y = g (k (q y))
           in MkIso @a @d u v left right
    iso2 :: Iso a d
    iso2 = MkIso @a @d u2 v2 left right
      where
        u2 x = p (h (f x))
        v2 y = g (k (q y))

-- | Where-clause inside a case branch and nested dependent calls.
packInCase
  :: forall a b c. Bool
  -> foreach (f :: a -> b) -> foreach (g :: b -> a)
  -> foreach (h :: b -> c) -> foreach (k :: c -> b)
  -> (foreach (x :: a) -> g (k (h (f x))) :~: x)
  -> (foreach (y :: c) -> h (f (g (k y))) :~: y)
  -> Iso a c
packInCase flag f g h k left right = case flag of
  True -> MkIso @a @c to from left right
    where
      to x = h (f x)
      from y = g (k y)
  False -> MkIso @a @c to from left right
    where
      to = h . f
      from = g . k

-- | Verify laziness when outer ignores inner result or input is bottom.
lazyCompose :: Int
lazyCompose =
  let f :: Int -> Int
      f = error "forced f"
      h :: Int -> Int
      h _ = 1
      to x = h (f x)
  in to (error "forced x")

composeLeft
  :: foreach (f :: a -> b) -> foreach (g :: b -> a)
  -> foreach (h :: b -> c) -> foreach (k :: c -> b)
  -> (foreach (x :: a) -> g (f x) :~: x)
  -> (foreach (y :: b) -> k (h y) :~: y)
  -> foreach (x :: a) -> g (k (h (f x))) :~: x
composeLeft f g _ _ gf kh x = trans (cong @g (kh (f x))) (gf x)

notInv6 :: foreach (x :: Bool)
        -> boolNot (boolNot (boolNot (boolNot (boolNot (boolNot x))))) :~: x
notInv6 True = Refl
notInv6 False = Refl

boolNot :: Bool -> Bool
boolNot True = False
boolNot False = True

notInv :: foreach (x :: Bool) -> boolNot (boolNot x) :~: x
notInv True = Refl
notInv False = Refl

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

intToBool :: Int -> Bool
intToBool 0 = False
intToBool _ = True

observeIso :: Iso Bool Bool -> (Bool, Bool, Bool)
observeIso (MkIso to from l r) =
  case l True of
    Refl -> case r False of
      Refl -> (to True, from False, to (from True))

main :: IO ()
main = do
  let i1 = packWhereSigned boolNot boolNot boolNot boolNot
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
      i2 = packWhereUnsigned boolNot boolNot boolNot boolNot
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
      i3 = packLet boolNot boolNot boolNot boolNot
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
      (i4, i5) = packForms boolNot boolNot boolNot boolNot
                   (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
                   (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
      (i6, i7) = packChained boolNot boolNot boolNot boolNot boolNot boolNot
                   notInv6 notInv6
      i8 = packInCase True boolNot boolNot boolNot boolNot
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
      i9 = packInCase False boolNot boolNot boolNot boolNot
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
             (composeLeft boolNot boolNot boolNot boolNot notInv notInv)
  print (map observeIso [i1, i2, i3, i4, i5, i6, i7, i8, i9])
  print (lazyCompose, boolToInt (intToBool 1))
