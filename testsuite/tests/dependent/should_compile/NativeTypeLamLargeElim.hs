{-# LANGUAGE DependentTypes #-}
module NativeTypeLamLargeElim where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import qualified GHC.Types as GHCTypes

-- 1. Inline type lambdas passed to promoted Prelude.map and local mapList
--    without TypeFamilies or UndecidableInstances.

data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

data List a = Nil | Cons a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Nil         = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

type Incremented = map (plus (Succ Zero)) [Zero, Succ Zero, Succ (Succ Zero)]

checkIncremented :: Incremented :~: [Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero))]
checkIncremented = Refl

type Doubled = map (\n -> plus n n) [Zero, Succ Zero, Succ (Succ Zero)]

checkDoubled :: Doubled :~: [Zero, Succ (Succ Zero), Succ (Succ (Succ (Succ Zero)))]
checkDoubled = Refl

type Applied = map (\f -> f Int) [Maybe, Either String, IO]

checkApplied :: Applied :~: [Maybe Int, Either String Int, IO Int]
checkApplied = Refl

type DoubledLocal =
  mapList (\n -> plus n n) (Cons Zero (Cons (Succ Zero) (Cons (Succ (Succ Zero)) Nil)))

checkDoubledLocal ::
  DoubledLocal :~: Cons Zero (Cons (Succ (Succ Zero)) (Cons (Succ (Succ (Succ (Succ Zero)))) Nil))
checkDoubledLocal = Refl

type AppliedLocal =
  mapList (\f -> f Int) (Cons Maybe (Cons (Either String) (Cons IO Nil)))

checkAppliedLocal ::
  AppliedLocal :~: Cons (Maybe Int) (Cons (Either String Int) (Cons (IO Int) Nil))
checkAppliedLocal = Refl

sigMapped :: Proxy (map (\n -> plus n n) [Zero, Succ Zero]) -> Proxy [Zero, Succ (Succ Zero)]
sigMapped x = x

sigMappedLocal ::
  Proxy (mapList (\n -> plus n n) (Cons Zero (Cons (Succ Zero) Nil))) ->
  Proxy (Cons Zero (Cons (Succ (Succ Zero)) Nil))
sigMappedLocal x = x

-- 2. Wildcard '_' and kind-annotated '(_ :: k)' / '(x :: k)' binders in type lambdas.

type ConstStrings = map (\_ -> String) [Int, Bool, Char]

checkConstStrings :: ConstStrings :~: [String, String, String]
checkConstStrings = Refl

type ConstNatStrings = map (\(_ :: Nat) -> String) [Zero, Succ Zero]

checkConstNatStrings :: ConstNatStrings :~: [String, String]
checkConstNatStrings = Refl

type AnnotatedDoubled = map (\(n :: Nat) -> plus n n) [Zero, Succ Zero]

checkAnnotatedDoubled :: AnnotatedDoubled :~: [Zero, Succ (Succ Zero)]
checkAnnotatedDoubled = Refl

type MultiWildcard = map (\_ -> (\_ -> Int) Zero) [Bool, Char]

checkMultiWildcard :: MultiWildcard :~: [Int, Int]
checkMultiWildcard = Refl

vtaWildcard :: Proxy (\(_ :: Nat) -> String)
vtaWildcard = Proxy @(\(_ :: Nat) -> String)

vtaMultiWildcard :: Proxy (\_ (_ :: Bool) -> Int)
vtaMultiWildcard = Proxy @(\_ (_ :: Bool) -> Int)

-- 3. Recursive term-to-Type large eliminations without 'type (...)',
--    with carried Type accumulators, and calling helper large eliminations.

data Seg = SUsers | SPosts

data Fmt1 = End1 | Lit1 Seg Fmt1 | D1 Fmt1 | S1 Fmt1

fmtType1 :: Fmt1 -> Type
fmtType1 End1       = String
fmtType1 (Lit1 _ r) = fmtType1 r
fmtType1 (D1 r)     = (Int -> fmtType1 r)
fmtType1 (S1 r)     = (String -> fmtType1 r)

checkFmtType1 :: fmtType1 (Lit1 SUsers (D1 (S1 End1))) :~: (Int -> String -> String)
checkFmtType1 = Refl

data Fmt = FEnd | FLit Fmt | FInt Fmt | FStr Fmt

fmtType :: Fmt -> Type -> Type
fmtType FEnd     acc = acc
fmtType (FLit r) acc = fmtType r acc
fmtType (FInt r) acc = (Int -> fmtType r acc)
fmtType (FStr r) acc = (String -> fmtType r acc)

checkFmtType :: fmtType (FLit (FInt (FStr FEnd))) Bool :~: (Int -> String -> Bool)
checkFmtType = Refl

runFmt :: foreach (f :: Fmt) -> String -> fmtType f String
runFmt FEnd     s = s
runFmt (FLit r) s = runFmt r (s ++ "/")
runFmt (FInt r) s = \n -> runFmt r (s ++ show n)
runFmt (FStr r) s = \t -> runFmt r (s ++ t)

data WireTy = WInt | WStr | WBool

interp :: WireTy -> Type
interp WInt  = Int
interp WStr  = String
interp WBool = Bool

type Schema = GHCTypes.List WireTy

linkType :: Schema -> Type -> Type
linkType []          res = res
linkType (ty : rest) res = type (interp ty -> linkType rest res)

checkLinkType :: linkType [WInt, WBool, WStr] Char :~: (Int -> Bool -> String -> Char)
checkLinkType = Refl

data Route = Get WireTy | Lit Seg Route | Cap WireTy Route

routeLink :: Route -> Type
routeLink (Get _)       = type String
routeLink (Lit _ rest)  = routeLink rest
routeLink (Cap ty rest) = type (interp ty -> routeLink rest)

checkRouteLink :: routeLink (Lit SUsers (Cap WInt (Cap WStr (Get WBool)))) :~: (Int -> String -> String)
checkRouteLink = Refl

routePayload :: Route -> Type
routePayload (Get ty)      = interp ty
routePayload (Lit _ rest)  = routePayload rest
routePayload (Cap ty rest) = (interp ty -> routePayload rest)

checkRoutePayload :: routePayload (Cap WInt (Get WBool)) :~: (Int -> Bool)
checkRoutePayload = Refl
