{-# LANGUAGE DependentTypes, ListTuplePuns, LinearTypes, TypeFamilies #-}
module NativeIndexedFinMissingAndMatrix where
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeIndexedFinGate
import PackedVecLibrary (Nat(..), Vec(..), sourceLength)
import ParameterizedIndexLibrary (Fin(..), indexGeneric)

type family Unknown (g :: Gate) :: Bool where
    Unknown ('Open b) = 'True

type family Always (g :: Gate) :: Bool where
    Always ('Open b) = 'True
    Always ('Shut b) = 'True

inhabited :: foreach (g :: Gate) -> Ticket (Always g) -> Bool
inhabited (Open b) Granted = b

stuck :: foreach (g :: Gate) -> Ticket (Unknown g) -> Bool
stuck (Open b) Granted = b

noSplit :: foreach (g :: Gate) -> Ticket (Ready g) -> Bool
noSplit (Open b) _ = b

lazySplit :: foreach (g :: Gate) -> Ticket (Ready g) -> Bool
lazySplit (Open b) ~Granted = b

data TwoPositions a b where
    Positions :: foreach (xs :: [a]) -> Fin (sourceLength xs) %1
              -> foreach (ys :: [b]) -> Fin (sourceLength ys) %1 -> TwoPositions a b

wrongBound :: TwoPositions a a %1 -> (a, a)
wrongBound (Positions xs@(_ : _) i ys@(_ : _) j) = (indexGeneric xs j, indexGeneric ys i)
wrongBound (Positions [] i _ _) = case i of {}
wrongBound (Positions (_ : _) _ [] j) = case j of {}

unrelatedLen :: forall n m a. foreach (xs :: [a]) -> (m ~ sourceLength xs) => Fin n %1 -> a
unrelatedLen (x : _) FZ = x
unrelatedLen (_ : xs) (FS i) = unrelatedLen xs i

onlyNil :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
        -> (sourceLength xs ~ sourceLength ys) => Vec (a, b) (sourceLength xs)
onlyNil [] [] = VNil

wrongMatrix :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
            -> (sourceLength xs ~ sourceLength ys) => Vec (a, b) (sourceLength xs)
wrongMatrix [] [] = VNil
wrongMatrix (_ : _) (_ : _) = VNil

unrelatedAmbient :: forall n p. (n ~ 'Z) => foreach (x :: Nat) -> foreach (y :: Nat)
                 -> (x ~ p, y ~ p) => Vec Bool x
unrelatedAmbient Z Z = VNil

discardLin :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) %1 -> (a -> a) %1 -> a
discardLin (x : _) FZ _ = x
discardLin (_ : rest) (FS i) k = discardLin rest i k

dupLin :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) %1 -> (a -> a) %1 -> a
dupLin (x : _) FZ k = k (k x)
dupLin (_ : rest) (FS i) k = dupLin rest i k

diffPred :: forall n. foreach (p :: Fin n) -> foreach (q :: Fin n) -> 'FS q :~: 'FS p
diffPred _ _ = Refl

remember :: foreach (xs :: [Int]) -> Proxy xs
remember _ = Proxy
sameContents :: foreach (xs :: [Int]) -> foreach (ys :: [Int]) -> (xs ~ ys) => Bool
sameContents _ _ = True
escapeField = let element = 1 :: Int in remember [element]
diffFields = let left = 1 :: Int; right = 2 :: Int in sameContents [left] [right]
