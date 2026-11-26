{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedPackageDepBad where
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl), (:~~:)(HRefl))
import GHC.Sigma (Sigma(Pair))
import NativeRetainedPackageProviders (Carrier, Linked(..))
import PackedProjection (Length)
import PackedVecLibrary (Nat(..), PackedVec(..), Vec(..), plus)
import RetainedNat (Retained(..))

wrongInline :: foreach (m :: Nat) -> foreach (n :: Nat) -> some (k :: Nat) | k :~: plus m n
wrongInline m n = Pair (S (plus m n)) Refl

type Body (a :: Type) r = foreach (x :: a) -> Proxy x -> r
type Indexed (a :: Type) = foreach (x :: a) -> Proxy x -> Proxy x
indexed :: Indexed (IO Int)
indexed _ value = value
takeBody :: Body (IO Int) r -> Proxy r
takeBody _ = Proxy
leak = takeBody indexed

wrongProjection :: foreach (p :: PackedVec a) -> Vec a ('S (Length p))
wrongProjection (PackVec _ xs) = xs

keepPkg :: foreach (p :: PackedVec Int) -> Proxy p
keepPkg _ = Proxy
consumePkg :: Proxy p -> Int
consumePkg _ = 0
wrongWitness :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int m -> Int
wrongWitness n _ xs = consumePkg (keepPkg (PackVec n xs))

wrongFamilyResult :: foreach (q :: Retained p) -> p 'Z
wrongFamilyResult (Retain _ value) = value

wrongVector :: foreach (xs :: Vec a n) -> foreach (ys :: Vec a n) -> xs :~: ys
wrongVector _ _ = Refl

wrongZero :: foreach (xs :: Vec Bool 'Z) -> foreach (ys :: Vec Bool 'Z) ->
             (xs :~: ('VNil :: Vec Bool 'Z), ys :~: ('VNil :: Vec Bool 'Z))
wrongZero xs _ = case xs of
    VNil -> (Refl, Refl)

crossed :: foreach (xs :: Vec Bool n) -> foreach (ys :: Vec Bool n) ->
    xs :~~: ('VNil :: Vec Bool 'Z) -> ('VNil :: Vec Bool 'Z) :~~: ys
crossed _ _ HRefl = HRefl

wrongLinked :: foreach (other :: Vec Bool 'Z) -> Linked n p q r -> Carrier other
wrongLinked _ (Linked (Pair _ (Pair _ (Pair _ (Pair _ (Pair _
    (p1, p2, p3, branch, leaf, back, _, pay))))))) =
    case p1 of
        HRefl -> case p2 of
            Refl -> case p3 of
                HRefl -> case branch of
                    HRefl -> case leaf of
                        HRefl -> case back of
                            HRefl -> pay
