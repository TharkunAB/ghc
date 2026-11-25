{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns, UnicodeSyntax, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
-- | Proposal #378 umbrella syntax, %U type-lambda captures, same-module
-- context/closure staging, inline GADT foreach arguments, Type large eliminations,
-- and dependent-base list/foldr/matchability laws.
module BookWave3Proposal378
  ( SumTwo
  , peanoToInt
  , plusAssocProd
  , sumTwoFirst
  , infixForEachArg
  , LeibnizU
  , leibnizUSym
  , leibnizUToEq
  , Fin (..)
  , UTerm (..)
  , UCtx (..)
  , plug
  , extU
  , renameU
  , extsU
  , substU
  , substSingleU
  , sameModulePlugBeta
  , plugCong
  , inlineHoleCong
  , Val (..)
  , valPred
  , applyValPred
  , addUnder
  , zipMapLaw
  , checkFoldrAndMatchability
  ) where

import Data.Kind (Type)
import qualified Data.List.Dependent as DepList
import qualified Data.List.Dependent.Properties as DepListProps
import Data.Nat.Dependent (Nat (..), minSize, plus)
import Data.Nat.Dependent.Properties (congFirst, congSucc, plusSucc, plusZero)
import Data.Type.Equality.Dependent ((:~:) (Refl), Matchability (..), U, cong, trans)
import GHC.Sigma (Sigma (Pair))
import GHC.Tuple (Unit)
import Prelude hiding (foldr)

-- | Convert a Peano natural to a machine integer for runtime display.
peanoToInt :: Nat -> Int
peanoToInt Zero     = 0
peanoToInt (Succ n) = 1 + peanoToInt n

-- | Prove addition associativity using Unicode product syntax and a shared-type telescope.
plusAssocProd :: ∏ (m n p :: Nat) -> plus (plus m n) p :~: plus m (plus n p)
plusAssocProd Zero     _ _ = Refl
plusAssocProd (Succ m) n p = congSucc (plusAssocProd m n p)

-- | Multi-binder retained sum whose components add to two.
type SumTwo = some (m k :: Nat) | plus m k :~: 2

-- | Read the first witness from a multi-binder retained sum.
sumTwoFirst :: Nat
sumTwoFirst = case sumTwoPair of
  Pair m _ -> m
  where
    sumTwoPair :: SumTwo
    sumTwoPair = Pair 1 (Pair 1 Refl)

-- | Accept a foreach quantifier directly in term syntax for a required type argument.
acceptForEachType :: forall (t :: Type) -> Nat
acceptForEachType _ = Succ Zero

-- | Pass a grouped foreach type directly without a type herald.
infixForEachArg :: Nat
infixForEachArg = acceptForEachType (foreach (m n :: Nat) -> plus m n :~: plus m n)

-- | Leibniz equality quantified over unmatchable predicates.
type LeibnizU (x :: a) (y :: a) = forall (p :: a %U -> Type). p x -> p y

-- | Invert Leibniz equality at a specific unmatchable predicate by capturing it in a type lambda.
leibnizUSymAt :: forall {a} (x y :: a) (p :: a %U -> Type).
                 LeibnizU x y -> p y -> p x
leibnizUSymAt xy = xy @(\z -> p z -> p x) (\px -> px)

-- | Invert Leibniz equality across all unmatchable predicates.
leibnizUSym :: forall {a} (x y :: a). LeibnizU x y -> LeibnizU y x
leibnizUSym xy = leibnizUSymAt @x @y xy

-- | Convert Leibniz equality to nominal propositional equality.
leibnizUToEq :: forall {a} (x y :: a). LeibnizU x y -> x :~: y
leibnizUToEq xy = xy @(\z -> x :~: z) Refl

-- | Bounded De Bruijn indices.
data Fin (n :: Nat) where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

-- | Scope-indexed untyped lambda terms.
data UTerm (n :: Nat) where
  UVar :: Fin n -> UTerm n
  ULam :: UTerm (Succ n) -> UTerm n

-- | Two-index context with a hole at scope m producing scope n.
data UCtx (m :: Nat) (n :: Nat) where
  CHole :: UCtx n n
  CLam  :: UCtx m (Succ n) -> UCtx m n

-- | Plug a term into a two-index context.
plug :: UCtx m n -> UTerm m -> UTerm n
plug CHole    t = t
plug (CLam c) t = ULam (plug c t)

-- | Lift a variable renaming across one binder.
extU :: (Fin m -> Fin n) -> Fin (Succ m) -> Fin (Succ n)
extU _ FZ     = FZ
extU r (FS x) = FS (r x)

-- | Rename variables in an untyped term.
renameU :: (Fin m -> Fin n) -> UTerm m -> UTerm n
renameU r (UVar x) = UVar (r x)
renameU r (ULam b) = ULam (renameU (extU r) b)

-- | Lift a substitution across one binder.
extsU :: (Fin m -> UTerm n) -> Fin (Succ m) -> UTerm (Succ n)
extsU _ FZ     = UVar FZ
extsU s (FS x) = renameU FS (s x)

-- | Apply a simultaneous substitution to an untyped term.
substU :: (Fin m -> UTerm n) -> UTerm m -> UTerm n
substU s (UVar x) = s x
substU s (ULam b) = ULam (substU (extsU s) b)

-- | Substitute one term for FZ inside the same module without a pragma flag.
substSingleU :: UTerm (Succ n) -> UTerm n -> UTerm n
substSingleU b v = substU (\x -> case x of { FZ -> v; FS y -> UVar y }) b

-- | Reduce both plug and substSingleU in a type inside their defining module.
sameModulePlugBeta
  :: substSingleU (plug CHole (UVar FZ :: UTerm (Succ Zero))) (ULam (UVar FZ))
     :~: ULam (UVar FZ)
sameModulePlugBeta = Refl

-- | Congruence of ULam with invisible term operands.
lamCong :: forall {n} {a b :: UTerm (Succ n)}. a :~: b -> ULam a :~: ULam b
lamCong Refl = Refl

-- | Prove context congruence without manual type applications on lamCong.
plugCong :: forall {m n} {t1 t2 :: UTerm m}.
            foreach (c :: UCtx m n) -> t1 :~: t2 -> plug c t1 :~: plug c t2
plugCong CHole    = \eq -> eq
plugCong (CLam c) = \eq -> lamCong (plugCong c eq)

-- | Pass an inline indexed GADT constructor directly to a foreach argument.
inlineHoleCong :: forall (t :: UTerm Zero). plug (CHole :: UCtx Zero Zero) t :~: t
inlineHoleCong @t = plugCong (CHole :: UCtx Zero Zero) (Refl @t)

-- | Simple type codes for large elimination.
data Val = Bot | Arr Val Val

-- | Compute a Haskell Type by structural recursion on Val.
valPred :: Val -> Type
valPred Bot       = type Unit
valPred (Arr a b) = type (valPred a -> valPred b)

-- | Apply a value whose function type is computed by valPred.
applyValPred :: valPred (Arr Bot (Arr Bot Bot)) -> Unit
applyValPred f = f () ()

-- | Move a successor through an inner addition under a fixed left operand.
addUnder :: foreach (m n p :: Nat) -> plus m (plus n ('Succ p)) :~: plus m ('Succ (plus n p))
addUnder m n p =
  cong (plus m) (plusSucc n p :: plus n ('Succ p) :~: 'Succ (plus n p))

-- | Compose lengthZipWith and lengthMap using trans and congFirst.
zipMapLaw :: forall a b c d. foreach (pre :: a -> b) -> foreach (combine :: b -> c -> d)
          -> foreach (xs :: [a]) -> foreach (ys :: [c])
          -> DepList.size (zipWith combine (map pre xs) ys) :~: minSize (DepList.size xs) (DepList.size ys)
zipMapLaw pre combine xs ys =
  DepListProps.lengthZipWith combine (map pre xs) ys
    `trans` congFirst @minSize @(DepList.size ys) (DepListProps.lengthMap pre xs)

type ArrowBy (m :: Matchability) (a :: Type) (b :: Type) = a %m -> b

matchableCong :: forall (f :: ArrowBy 'Matchable Nat Nat) x y. x :~: y -> f x :~: f y
matchableCong = cong f

unmatchableCong :: forall (f :: ArrowBy 'Unmatchable Nat Nat) x y. x :~: y -> f x :~: f y
unmatchableCong = cong f

foldrPlusAppend :: foreach (xs ys :: [Nat]) -> foreach (z :: Nat)
                -> DepList.foldr plus z (xs ++ ys) :~: DepList.foldr plus (DepList.foldr plus z ys) xs
foldrPlusAppend []       _  _ = Refl
foldrPlusAppend (x : xs) ys z = cong (plus x) (foldrPlusAppend xs ys z)

foldrPlusZero :: foreach (xs :: [Nat])
              -> plus (DepList.foldr plus 'Zero xs) 'Zero :~: DepList.foldr plus 'Zero xs
foldrPlusZero xs = plusZero (DepList.foldr plus Zero xs)

-- | Exercise checked foldr laws and both Matchability constructors.
checkFoldrAndMatchability :: [Nat] -> [Nat] -> Bool
checkFoldrAndMatchability xs ys =
  case foldrPlusAppend xs ys Zero of
    Refl -> case foldrPlusZero xs of
      Refl -> case (matchableCong @'Succ (Refl @'Zero), unmatchableCong @(plus 'Zero) (Refl @'Zero)) of
        (Refl, Refl) -> True
