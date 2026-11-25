{-# LANGUAGE DependentTypes #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl), sym)
import qualified GHC.Tuple as Tup
import GHC.Types (List)
import NativeRetainedScrutineePolyProvider

appendNilR :: forall a. foreach (xs :: List a) -> appList xs [] :~: xs
appendNilR [] = Refl
appendNilR (_ : xs) = case appendNilR xs of
  Refl -> Refl

appendNilRInvis :: forall a. foreach (xs :: List a). appList xs [] :~: xs
appendNilRInvis @_ @xs = appendNilR xs

appendAssoc :: forall a. foreach (xs ys zs :: List a) ->
               appList (appList xs ys) zs :~: appList xs (appList ys zs)
appendAssoc [] _ _ = Refl
appendAssoc (_ : xs) ys zs = case appendAssoc xs ys zs of
  Refl -> Refl

pairNilR :: forall a b. foreach (xs :: List a) -> foreach (ys :: List b) ->
            (appList xs [], appList ys []) :~: (xs, ys)
pairNilR xs ys =
  case appendNilR xs of
    Refl -> case appendNilR ys of
      Refl -> Refl

withExtra :: forall a b. foreach (xs :: List a) -> b -> appList xs [] :~: xs
withExtra [] _ = Refl
withExtra (_ : xs) v = case withExtra xs v of
  Refl -> Refl

-- 1. Direct PiTA Entry 5 reproducer (polymorphic ys :: [a]):
withRev :: forall a. foreach (ys :: List a) -> appList (rev ys) [] :~: rev ys
withRev ys = appendNilR (rev ys)

withRevCase :: forall a. foreach (ys :: List a) -> appList (rev ys) [] :~: rev ys
withRevCase ys = case appendNilR (rev ys) of
  Refl -> Refl

-- 2. Concrete element type (ys :: [Bool]):
withRevCaseBool :: foreach (ys :: List Bool) -> appList (rev ys) [] :~: rev ys
withRevCaseBool ys = case appendNilR (rev ys) of
  Refl -> Refl

withFlipRevBool :: foreach (ys :: List Bool) -> appList (mapFlip (rev ys)) [] :~: mapFlip (rev ys)
withFlipRevBool ys = case appendNilR (mapFlip (rev ys)) of
  Refl -> Refl

-- 3. Nested inferred call in case scrutinee:
withRevSymCase :: forall a. foreach (ys :: List a) -> rev ys :~: appList (rev ys) []
withRevSymCase ys = case sym (appendNilR (rev ys)) of
  Refl -> Refl

-- 4. Let-bound inferred call scrutinized by case:
withRevLetCase :: forall a. foreach (ys :: List a) -> appList (rev ys) [] :~: rev ys
withRevLetCase ys =
  let p = appendNilR (rev ys)
  in case p of
       Refl -> Refl

-- 5. Nested promoted calls rev (rev ys) and wildcard @_ VTA in case scrutinee:
withRevTwiceCase :: forall a. foreach (ys :: List a) -> appList (rev (rev ys)) [] :~: rev (rev ys)
withRevTwiceCase ys = case appendNilR (rev (rev ys)) of
  Refl -> Refl

withRevWildcardVta :: forall a. foreach (ys :: List a) -> appList (rev ys) [] :~: rev ys
withRevWildcardVta ys = case appendNilR @_ (rev ys) of
  Refl -> Refl

-- 6. Invisible foreach . with @arg override in case scrutinee:
withRevInvisCase :: forall a. foreach (ys :: List a) -> appList (rev ys) [] :~: rev ys
withRevInvisCase ys = case appendNilRInvis @_ @(rev ys) of
  Refl -> Refl

-- 7. Multi-argument polymorphic foreach calls in case scrutinees:
withRevAssocCase :: forall a. foreach (xs ys zs :: List a) ->
                    appList (appList (rev xs) (rev ys)) zs :~: appList (rev xs) (appList (rev ys) zs)
withRevAssocCase xs ys zs = case appendAssoc (rev xs) (rev ys) zs of
  Refl -> Refl

withPairCase :: forall a. foreach (xs :: List a) -> foreach (bs :: List Bool) ->
                (appList (rev xs) [], appList (mapFlip bs) [])
                  :~: (rev xs, mapFlip bs)
withPairCase xs bs = case pairNilR (rev xs) (mapFlip bs) of
  Refl -> Refl

-- 8. Unrelated undetermined type parameter alongside polymorphic foreach scrutinee:
withExtraCase :: forall a. foreach (ys :: List a) -> appList (rev ys) [] :~: rev ys
withExtraCase ys = case withExtra (rev ys) True of
  Refl -> Refl

-- 9. Element type containing a runtime index (ys :: [Vec Int n]):
withIndexedElemCase :: foreach (n :: Nat) -> foreach (ys :: List (Vec Int n)) ->
                       appList @(Vec Int n) (rev ys) ([] @(Vec Int n)) :~: rev ys
withIndexedElemCase _ ys = case appendNilR (rev ys) of
  Refl -> Refl

-- 10. Prelude (++) and rev:
appendNilRBase :: forall a. foreach (xs :: List a) -> (xs ++ []) :~: xs
appendNilRBase [] = Refl
appendNilRBase (_ : xs) = case appendNilRBase xs of
  Refl -> Refl

withRevBaseCase :: forall a. foreach (ys :: List a) -> (rev ys ++ []) :~: rev ys
withRevBaseCase ys = case appendNilRBase (rev ys) of
  Refl -> Refl

main :: IO Tup.Unit
main = do
  let nums :: List Int
      nums = [1, 2, 3]
      nums2 :: List Int
      nums2 = [4]
      nums3 :: List Int
      nums3 = [5, 6]
      bools :: List Bool
      bools = [True, False]
      vecs :: List (Vec Int (Succ Zero))
      vecs = [VCons 7 VNil]
      r1 = case withRev nums of Refl -> 1 :: Int
      r2 = case withRevCase nums of Refl -> 2 :: Int
      r3 = case withRevCaseBool bools of Refl -> 3 :: Int
      r4 = case withFlipRevBool bools of Refl -> 4 :: Int
      r5 = case withRevSymCase nums of Refl -> 5 :: Int
      r6 = case withRevLetCase nums of Refl -> 6 :: Int
      r7 = case withRevTwiceCase nums of Refl -> 7 :: Int
      r8 = case withRevWildcardVta nums of Refl -> 8 :: Int
      r9 = case withRevInvisCase nums of Refl -> 9 :: Int
      r10 = case withRevAssocCase nums nums2 nums3 of Refl -> 10 :: Int
      r11 = case withPairCase nums bools of Refl -> 11 :: Int
      r12 = case withExtraCase nums of Refl -> 12 :: Int
      r13 = case withIndexedElemCase (Succ Zero) vecs of Refl -> 13 :: Int
      r14 = case withRevBaseCase nums of Refl -> 14 :: Int
  print (r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14)
