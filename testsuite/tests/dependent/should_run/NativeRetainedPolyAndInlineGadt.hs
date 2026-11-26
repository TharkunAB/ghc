{-# LANGUAGE AllowAmbiguousTypes, DataKinds, ExplicitNamespaces, DependentTypes, ListTuplePuns, GADTs, KindSignatures, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module Main where

import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeRetainedPolyAndInlineGadtProvider as P

plugHoleId :: forall (t :: P.UTerm 'P.Zero).
              foreach (c :: P.UCtx 'P.Zero 'P.Zero) ->
              Maybe (P.plug c t :~: t)
plugHoleId P.CHole    = Just Refl
plugHoleId (P.CLam _) = Nothing
plugHoleId (P.CAppL _ _) = Nothing
plugHoleId (P.CAppR _ _) = Nothing

checkDirectAnnot :: Maybe (P.plug (P.CHole :: P.UCtx 'P.Zero 'P.Zero)
                                  (P.ULam (P.UVar P.FZ) :: P.UTerm 'P.Zero)
                          :~: ('P.ULam ('P.UVar 'P.FZ) :: P.UTerm 'P.Zero))
checkDirectAnnot =
  plugHoleId @(P.ULam (P.UVar P.FZ) :: P.UTerm 'P.Zero)
             (P.CHole :: P.UCtx 'P.Zero 'P.Zero)

checkDirectBare :: Maybe (('P.ULam ('P.UVar 'P.FZ) :: P.UTerm 'P.Zero)
                          :~: ('P.ULam ('P.UVar 'P.FZ) :: P.UTerm 'P.Zero))
checkDirectBare =
  plugHoleId @(P.ULam (P.UVar P.FZ) :: P.UTerm 'P.Zero) P.CHole

data ChainWitness (m :: P.Nat) (n :: P.Nat) :: P.Chain m n -> Type where
  MkChainWitness :: foreach (ch :: P.Chain m n) -> ChainWitness m n ch

checkExistentialGadtArg :: Bool
checkExistentialGadtArg =
  case MkChainWitness
         (P.CStep (P.CRefl :: P.Chain 'P.Zero 'P.Zero)
                  (P.CRefl :: P.Chain 'P.Zero 'P.Zero)
           :: P.Chain 'P.Zero 'P.Zero) of
    MkChainWitness _ -> True

data Rel (n :: P.Nat) :: P.UTerm n -> P.UTerm n -> Type where
  MkRel :: Rel n t1 t2

lamCong :: forall n (b1 :: P.UTerm ('P.Succ n)) (b2 :: P.UTerm ('P.Succ n)).
           Rel ('P.Succ n) b1 b2 -> Rel n ('P.ULam b1) ('P.ULam b2)
lamCong MkRel = MkRel

appCong :: forall n (l1 :: P.UTerm n) (l2 :: P.UTerm n) (r1 :: P.UTerm n) (r2 :: P.UTerm n).
           Rel n l1 l2 -> Rel n r1 r2 -> Rel n ('P.UApp l1 r1) ('P.UApp l2 r2)
appCong MkRel MkRel = MkRel

transRel :: forall n (t1 :: P.UTerm n) (t2 :: P.UTerm n) (t3 :: P.UTerm n).
            Rel n t1 t2 -> Rel n t2 t3 -> Rel n t1 t3
transRel MkRel MkRel = MkRel

comp :: forall m n (t1 :: P.UTerm m) (t2 :: P.UTerm m).
        foreach (c :: P.UCtx m n) -> Rel m t1 t2 -> Rel n (P.plug c t1) (P.plug c t2)
comp P.CHole       = \eq -> eq
comp (P.CLam c)    = \eq -> lamCong (comp @m @('P.Succ n) @t1 @t2 c eq)
comp (P.CAppL c _) = \eq -> appCong (comp @m @n @t1 @t2 c eq) MkRel
comp (P.CAppR _ c) = \eq -> appCong MkRel (comp @m @n @t1 @t2 c eq)

compNoVta :: forall m n (t1 :: P.UTerm m) (t2 :: P.UTerm m).
             foreach (c :: P.UCtx m n) -> Rel m t1 t2 -> Rel n (P.plug c t1) (P.plug c t2)
compNoVta P.CHole       = \eq -> eq
compNoVta (P.CLam c)    = \eq -> lamCong (compNoVta c eq)
compNoVta (P.CAppL c _) = \eq -> appCong (compNoVta c eq) MkRel
compNoVta (P.CAppR _ c) = \eq -> appCong MkRel (compNoVta c eq)

compTrans :: forall m n (t1 :: P.UTerm m) (t2 :: P.UTerm m) (t3 :: P.UTerm m).
             foreach (c :: P.UCtx m n) -> Rel m t1 t2 -> Rel m t2 t3
          -> Rel n (P.plug c t1) (P.plug c t3)
compTrans c eq1 eq2 = transRel (compNoVta c eq1) (compNoVta c eq2)

checkInlinePolyForEach :: Rel 'P.Zero ('P.ULam ('P.UVar 'P.FZ)) ('P.ULam ('P.UVar 'P.FZ))
                       -> Rel ('P.Succ 'P.Zero) ('P.UVar 'P.FZ) ('P.UVar 'P.FZ)
                       -> (Bool, Bool)
checkInlinePolyForEach eq0 eq1 =
  let b1 = case comp (P.CHole :: P.UCtx 'P.Zero 'P.Zero) eq0 of
             MkRel -> True
      b2 = case compNoVta
                  (P.CLam (P.CAppR (P.UVar P.FZ :: P.UTerm ('P.Succ 'P.Zero))
                                   (P.CHole :: P.UCtx ('P.Succ 'P.Zero) ('P.Succ 'P.Zero)))
                    :: P.UCtx ('P.Succ 'P.Zero) 'P.Zero)
                  eq1 of
             MkRel -> True
  in (b1, b2)

main :: IO ()
main = do
  let eq0 :: Rel 'P.Zero ('P.ULam ('P.UVar 'P.FZ)) ('P.ULam ('P.UVar 'P.FZ))
      eq0 = MkRel
      eq1 :: Rel ('P.Succ 'P.Zero) ('P.UVar 'P.FZ) ('P.UVar 'P.FZ)
      eq1 = MkRel
      ctx :: P.UCtx ('P.Succ 'P.Zero) 'P.Zero
      ctx = P.CLam (P.CAppL (P.CAppR (P.UVar P.FZ) P.CHole) (P.UVar P.FZ))
      r1 = case checkDirectAnnot of Just Refl -> True; Nothing -> False
      r2 = case checkDirectBare of Just Refl -> True; Nothing -> False
      r3 = checkExistentialGadtArg
      r4 = case comp ctx eq1 of
             MkRel -> True
      r5 = case compTrans ctx eq1 eq1 of
             MkRel -> True
      (r6, r7) = checkInlinePolyForEach eq0 eq1
  print (r1, r2, r3, r4, r5, r6, r7)
