{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ExplicitNamespaces #-}
{-# LANGUAGE GADTs, ListTuplePuns, PartialTypeSignatures, PolyKinds, RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments, ScopedTypeVariables, TypeAbstractions #-}
{-# LANGUAGE TypeApplications, TypeFamilies, UnicodeSyntax #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main (main) where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Exists
import GHC.Exts (U)
import GHC.Sigma (Sigma (Pair))
import GHC.Types (Matchability)
import NativeProposal378PatternsAndT2TProvider

--------------------------------------------------------------------------------
-- 1. P1.4: Full T2T lowering for foreach, ∏, exists, and some in required args
--------------------------------------------------------------------------------

idReq :: forall (t :: Type) -> t -> t
idReq _ x = x

idRep :: foreach (t :: Type) -> t -> t
idRep _ x = x

zeroVec :: foreach (n :: Nat) -> Vec Int n
zeroVec Zero     = VNil
zeroVec (Succ k) = VCons 0 (zeroVec k)

sumVec :: ∏ (m n :: Nat) -> Vec Int (plus m n)
sumVec m n = zeroVec (plus m n)

testT2T :: ([Int], [Int], Int, [Int])
testT2T =
  let e1 = idReq (exists {k} (x :: k). Proxy x) (Proxy @True)
      s1 = idRep (some (m :: Nat) | Vec Int m) (Pair (Succ Zero) (VCons 17 VNil))
      c1 = case e1 of { (Proxy :: Proxy x) -> 1 :: Int }
      c2 = case s1 of { Pair _ xs -> vecToList xs }
  in ( vecToList (idReq (foreach (n :: Nat) -> Vec Int n) zeroVec (Succ (Succ Zero)))
     , vecToList (idRep (∏ (m n :: Nat) -> Vec Int (plus m n)) sumVec (Succ Zero) (Succ Zero))
     , c1
     , c2 )

--------------------------------------------------------------------------------
-- 2. P1.12.2: Uncaptured `some` in arbitrary type positions
--------------------------------------------------------------------------------

data WrappedSome where
  WrapSome :: (some (n :: Nat) | Vec Int n) -> WrappedSome

localPolySome :: forall a. a -> [a]
localPolySome x =
  let s1 :: some (n :: Nat) | Vec a n
      s1 = Pair (Succ (Succ Zero)) (VCons x (VCons x VNil))
      s2 = (Pair (Succ Zero) (VCons 99 VNil) :: some (n :: Nat). Vec Int n)
      s3 :: some (m n :: Nat) | Vec a (plus m n)
      s3 = Pair (Succ Zero) (Pair (Succ Zero) (VCons x (VCons x VNil)))
      w  = WrapSome (Pair (Succ Zero) (VCons 5 VNil))
  in case (s1, s2, s3, w, importedSomeVec x) of
       (Pair _ xs, Pair _ _, Pair _ (Pair _ ys), WrapSome (Pair _ _), Pair _ zs) ->
         vecToList xs ++ vecToList ys ++ vecToList zs

--------------------------------------------------------------------------------
-- 3a. P1.9: 3+ foreach columns, catch-all fallback, & nested Succ (Succ k)
--------------------------------------------------------------------------------

type family TriFam (a :: Nat) (b :: Nat) (c :: Nat) :: Type where
  TriFam Zero     Zero     Zero     = Int
  TriFam (Succ _) Zero     Zero     = Bool
  TriFam _        _        _        = String

triMatch :: foreach (a b c :: Nat) -> TriFam a b c
triMatch Zero     Zero     Zero     = 42
triMatch (Succ _) Zero     Zero     = True
triMatch _        (Succ _) _        = if triFallback Zero (Succ Zero) Zero == Zero then "other" else "bad"
triMatch _        Zero     (Succ _) = "other"

triFallback :: foreach (a b c :: Nat) -> Nat
triFallback Zero     Zero     Zero = Succ Zero
triFallback (Succ k) Zero     Zero = k
triFallback _        _        _    = Zero

oneColFallback :: foreach (n :: Nat) -> Vec Int n -> [Int]
oneColFallback Zero VNil = [0]
oneColFallback _    xs   = vecToList xs

depCaseFallback :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
depCaseFallback n v =
  case n of
    Zero -> VNil
    _    -> v

type family Step2 (n :: Nat) :: Type where
  Step2 Zero            = Int
  Step2 (Succ Zero)     = Bool
  Step2 (Succ (Succ _)) = String

nestedCase :: foreach (n :: Nat) -> Step2 n
nestedCase n =
  case n of
    Zero          -> 10
    Succ Zero     -> True
    Succ (Succ _) -> "two+"

nestedCaseFallback :: foreach (n :: Nat) -> Vec Int n -> Vec Int n
nestedCaseFallback n v =
  case n of
    Succ (Succ k) -> VCons 100 (VCons 200 (zeroVec k))
    _             -> v

nestedForEach :: foreach (n :: Nat) -> Vec Int n -> Int
nestedForEach Zero            VNil                  = 0
nestedForEach (Succ Zero)     (VCons x VNil)        = x
nestedForEach (Succ (Succ _)) (VCons x (VCons y _)) = x + y

nestedLazyFallback :: foreach (n :: Nat) -> Int -> Nat
nestedLazyFallback (Succ (Succ k)) _ = k
nestedLazyFallback _               _ = Zero

--------------------------------------------------------------------------------
-- 3b. P1.9: Retained GADT constructors (MkT, MkShow, MkInvis)
--------------------------------------------------------------------------------

testRetainedGadts :: (Nat, Nat, String, (Nat, [Bool]), [Int], (Nat, [Int]), [Int])
testRetainedGadts =
  let tVal = MkT Int (Succ (Succ Zero)) (VCons 10 (VCons 20 VNil))
      n1 = matchImportedT tVal
      n2 = case tVal of
             MkT a n (xs :: Vec a n) -> plus n (case xs of { VNil -> Zero; VCons _ _ -> Succ Zero })
      sv = MkShow (Succ (Succ Zero)) (VCons True (VCons False VNil))
      sStr = matchImportedShow sv
      iv1 = matchImportedInvis mkInvisTrue
      iv2 :: InvisVec Int
      iv2 = MkInvis @Int @(Succ Zero) (VCons 77 VNil)
      xs2 = case iv2 of
              MkInvis @_ @(Succ Zero) (VCons x VNil) -> [x]
              MkInvis _ -> []
              MkInvisBrace _ -> []
              MkInvisInf _ -> []
      ivBrace :: InvisVec Int
      ivBrace = MkInvisBrace @(Succ Zero) (VCons 66 VNil)
      xsBrace = case ivBrace of
                  MkInvisBrace @n xs -> (n, vecToList xs)
                  _ -> (Zero, [])
      iv3 :: InvisVec Int
      iv3 = MkInvisInf (VCons 88 VNil)
      xs3 = case iv3 of
              MkInvisInf xs -> vecToList xs
              _ -> []
  in (n1, n2, sStr, iv1, xs2, xsBrace, xs3)

--------------------------------------------------------------------------------
-- 3c. P1.9: Rank-2 DenEq inference inside \eq -> lamCong (compositionality c eq)
--------------------------------------------------------------------------------

data UTerm (n :: Nat) where
  UVar :: UTerm (Succ n)
  ULam :: UTerm (Succ n) -> UTerm n
  UApp :: UTerm n -> UTerm n -> UTerm n

data UCtx (m :: Nat) (n :: Nat) where
  CHole :: UCtx n n
  CLam  :: UCtx m (Succ n) -> UCtx m n
  CAppL :: UCtx m n -> UTerm n -> UCtx m n
  CAppR :: UTerm n -> UCtx m n -> UCtx m n

plug :: UCtx m n -> UTerm m -> UTerm n
plug CHole       t = t
plug (CLam c)    t = ULam (plug c t)
plug (CAppL c r) t = UApp (plug c t) r
plug (CAppR l c) t = UApp l (plug c t)

data ValEnv (n :: Nat) where
  VENil :: ValEnv Zero
  VECons :: ValEnv n -> Int -> ValEnv (Succ n)

data ValW (v :: Nat) where
  MkValW :: ValW v

newtype Den (t :: UTerm n) (e :: ValEnv n) (v :: Nat) = MkDen Int

type DenEq (d1 :: ValEnv n -> Nat -> Type) (d2 :: ValEnv n -> Nat -> Type) =
  forall (e :: ValEnv n) (v :: Nat). ValW v -> (d1 e v -> d2 e v, d2 e v -> d1 e v)

denEqRefl :: DenEq d d
denEqRefl _ = (id, id)

lamCong :: forall (n :: Nat) (b :: UTerm (Succ n)) (b' :: UTerm (Succ n)).
           DenEq (Den b) (Den b') -> DenEq (Den (ULam b)) (Den (ULam b'))
lamCong _ _ = (\(MkDen k) -> MkDen (k + 10), \(MkDen k) -> MkDen (k - 10))

appCong :: forall (n :: Nat) (l :: UTerm n) (l' :: UTerm n) (m :: UTerm n) (m' :: UTerm n).
           DenEq (Den l) (Den l') -> DenEq (Den m) (Den m')
        -> DenEq (Den (UApp l m)) (Den (UApp l' m'))
appCong _ _ _ = (\(MkDen k) -> MkDen (k + 1), \(MkDen k) -> MkDen (k - 1))

-- Zero @ annotations needed on lamCong, appCong, or compositionality!
compositionality
  :: forall (m :: Nat) (n :: Nat) (t1 :: UTerm m) (t2 :: UTerm m).
     foreach (c :: UCtx m n)
  -> DenEq (Den t1) (Den t2)
  -> DenEq (Den (plug c t1)) (Den (plug c t2))
compositionality CHole       = \eq -> eq
compositionality (CLam c)    = \eq -> lamCong (compositionality c eq)
compositionality (CAppL c _) = \eq -> appCong (compositionality c eq) denEqRefl
compositionality (CAppR _ c) = \eq -> appCong denEqRefl (compositionality c eq)

data FinG (n :: Nat) where
  FZG :: FinG (Succ n)
  FSG :: FinG n -> FinG (Succ n)

type family LiftFin (f :: FinG m %U -> Nat) (y :: FinG (Succ m)) :: Nat where
  LiftFin _ FZG     = Zero
  LiftFin f (FSG x) = plus (Succ Zero) (f x)

congReq :: forall {a} {b} {m :: Matchability}.
           forall (f :: a %m -> b) -> forall (x :: a) (y :: a).
           x :~: y -> f x :~: f y
congReq _ Refl = Refl

eqZeroFin :: foreach (x :: FinG (Succ Zero)) -> (\_z -> Zero) x :~: (\_z -> Zero) x
eqZeroFin FZG     = Refl
eqZeroFin (FSG x) = case x of {}

congLiftFin
  :: forall (m :: Nat) (f1 :: FinG m %U -> Nat) (f2 :: FinG m %U -> Nat).
     (foreach (x :: FinG m) -> f1 x :~: f2 x)
  -> foreach (y :: FinG (Succ m)) -> LiftFin f1 y :~: LiftFin f2 y
congLiftFin _  FZG     = Refl
congLiftFin eq (FSG x) = congReq (plus (Succ Zero)) (eq x)

testRank2Compositional :: Int
testRank2Compositional =
  let ctx :: UCtx (Succ Zero) Zero
      ctx = CLam (CAppL CHole UVar)
      eq0 :: DenEq (Den UVar) (Den UVar)
      eq0 = denEqRefl
      MkDen out = fst (compositionality ctx eq0 MkValW) (MkDen 5)
      liftOk = case congLiftFin @(Succ Zero) @(\_z -> Zero) @(\_z -> Zero) eqZeroFin (FSG FZG) of
                 Refl -> 0
  in out + liftOk

--------------------------------------------------------------------------------
-- 4. P1.8 Local Scoping / Partial Sigs & #473 @Pack
--------------------------------------------------------------------------------

testLocalScopingAndPartialSigs :: ([Int], [Bool])
testLocalScopingAndPartialSigs =
  let n = Succ (Succ Zero)
      v1 :: Vec Int n
      v1 = VCons 3 (VCons 4 VNil)
      v2 :: Vec _ n
      v2 = VCons True (VCons False VNil)
  in (vecToList v1, vecToList v2)

showExplicitAtPack :: (exists a | (a, a -> String)) -> String
showExplicitAtPack (Pack @a (x, render)) = render (x :: a)

testAtPack :: (String, String, Int, Int)
testAtPack =
  let p1 :: exists a | (a, a -> String)
      p1 = Pack @Int (42, show)
      s1 = showExplicitAtPack p1
      s2 = case (Pack @Bool (True, show) :: exists a | (a, a -> String)) of
             Pack @_ (flag, render) -> render flag
      pDot :: exists a. Vec a (Succ Zero)
      pDot = Pack @Char (VCons 'z' VNil)
      c1 = case pDot of
             Pack @elemTy (xs :: Vec elemTy (Succ Zero)) -> length (vecToList xs)
      fnDot :: (exists a. Vec a (Succ Zero)) -> Int
      fnDot = \(Pack @elemTy (xs :: Vec elemTy (Succ Zero))) -> length (vecToList xs) + 1
  in (s1, s2, c1, fnDot pDot)

main :: IO ()
main = print
  ( testT2T
  , localPolySome 'q'
  , ( triMatch Zero Zero Zero
    , triMatch (Succ Zero) Zero Zero
    , triMatch Zero (Succ Zero) (Succ Zero)
    , oneColFallback Zero VNil
    , oneColFallback (Succ Zero) (VCons 8 VNil)
    , vecToList (depCaseFallback (Succ Zero) (VCons 9 VNil))
    , (nestedCase Zero, nestedCase (Succ Zero), nestedCase (Succ (Succ Zero)))
    , ( vecToList (nestedCaseFallback (Succ Zero) (VCons 7 VNil))
      , vecToList (nestedCaseFallback (Succ (Succ Zero)) (VCons 1 (VCons 2 VNil))) )
    , ( nestedForEach Zero VNil
      , nestedForEach (Succ Zero) (VCons 11 VNil)
      , nestedForEach (Succ (Succ Zero)) (VCons 11 (VCons 22 VNil)) )
    , ( nestedLazyFallback Zero (error "unused lazy arg")
      , nestedLazyFallback (Succ (Succ (Succ Zero))) (error "unused lazy arg 2") )
    )
  , testRetainedGadts
  , testRank2Compositional
  , testLocalScopingAndPartialSigs
  , testAtPack
  )
