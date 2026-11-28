{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns #-}
module NativeRetainedCastDomainBranchWrong where
import Data.Kind (Type)
import PackedVecLibrary (Add, Nat(..), Vec)

type BranchVector :: forall n. Vec Bool n -> Type
data BranchVector xs where
    BranchVector :: BranchVector xs

observeVector :: forall n. foreach (xs :: Vec Bool n) -> BranchVector xs
observeVector _ = BranchVector

wrongBranchLength :: forall m. foreach (cut :: Nat) -> foreach (xs :: Vec Bool (Add cut m)) -> BranchVector xs
wrongBranchLength Z xs = observeVector @('S m) xs
wrongBranchLength (S k) xs = observeVector @('S (Add k m)) xs

wrongBranchPair :: foreach (n :: Nat) -> foreach (xs :: Vec Bool n) -> foreach (ys :: Vec Bool n)
                -> (BranchVector xs, BranchVector ys)
wrongBranchPair n xs ys = case n of
    Z -> (observeVector @'Z xs, observeVector @'Z ys)
    S k -> (observeVector @('S k) ys, observeVector @('S k) xs)
