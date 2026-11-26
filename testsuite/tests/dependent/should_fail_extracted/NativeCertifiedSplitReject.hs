{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeCertifiedSplitReject where
import CertifiedSplit
  ( SplitAtValue, splitAtValue
  , CheckedSplitAtValue, SplitAtWidth(..)
  , CheckedSplitBoth, SplitBothAtWidth(..)
  )
import Data.Type.Equality ((:~:)(Refl))
import GHC.Sigma (Sigma(Pair))
import qualified PackedVecLibrary as P

missingPremise :: forall a m. foreach (cut :: P.Nat) -> foreach (v :: P.Vec a (P.Add cut m))
               -> SplitAtValue a cut m v
missingPremise P.Z v = Pair P.VNil (Pair v Refl)
missingPremise (P.S k) (P.VCons x xs) =
    case missingPremise @a @m k xs of
        Pair left (Pair right _premise) ->
            Pair (P.VCons x left) (Pair right Refl)

otherVector :: forall a. foreach (cut :: P.Nat) -> foreach (total :: P.Nat)
            -> foreach (v :: P.Vec a total) -> foreach (other :: P.Vec a total)
            -> Maybe (CheckedSplitAtValue a cut total v)
otherVector cut total _ other = case P.cutRemainder cut total of
    Nothing -> Nothing
    Just (Pair suffix Refl) ->
        Just (Pair suffix (SplitAtWidth (splitAtValue cut other)))

swappedVectors :: forall a. foreach (cut :: P.Nat) -> foreach (total :: P.Nat)
               -> foreach (xs :: P.Vec a total) -> foreach (ys :: P.Vec a total)
               -> Maybe (CheckedSplitBoth a a cut total xs ys)
swappedVectors cut total xs ys = case P.cutRemainder cut total of
    Nothing -> Nothing
    Just (Pair suffix Refl) ->
        Just (Pair suffix (SplitBothAtWidth
            (splitAtValue cut ys) (splitAtValue cut xs)))

noLengthProof :: forall a b. foreach (cut :: P.Nat) -> foreach (total :: P.Nat)
              -> foreach (xs :: P.Vec a total) -> foreach (ys :: P.Vec b total)
              -> Maybe (CheckedSplitBoth a b cut total xs ys)
noLengthProof cut total xs ys = case P.cutRemainder cut total of
    Nothing -> Nothing
    Just (Pair suffix _) ->
        Just (Pair suffix (SplitBothAtWidth
            (splitAtValue cut xs) (splitAtValue cut ys)))
