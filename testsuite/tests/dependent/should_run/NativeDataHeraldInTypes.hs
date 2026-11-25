{-# LANGUAGE DependentTypes, NoListTuplePuns #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Main (main) where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:)(Refl))
import qualified GHC.Tuple as Tup
import GHC.Types (List)
import NativeDataHeraldInTypesProvider

-- 1. Punned data T = T:
--    Proxy T refers to type T :: Type, while Proxy (data T) refers to 'T :: T.
proxyTypeT :: Proxy (T :: Type)
proxyTypeT = Proxy :: Proxy T

proxyDataT :: Proxy (data (T :: T))
proxyDataT = Proxy :: Proxy (data T)

-- 2. Standard constructors without tick syntax:
proxyDataTrue :: Proxy (data True)
proxyDataTrue = Proxy

vecTwo :: a -> Vec a (data (Succ (Succ Zero)))
vecTwo x = VCons x (VCons x (VNil :: Vec a (data Zero)))

vecStep :: foreach (n :: Nat) -> a -> Vec a (data n) -> Vec a (data (Succ n))
vecStep _ x xs = VCons x xs

-- 3. Punned term variable x vs enclosing forall x.:
punTermVar :: foreach (x :: Nat) -> Vec Char (data x) -> Nat
punTermVar x _ = helper True (Proxy :: Proxy (data x))
  where
    helper :: forall x. x -> Proxy (data x) -> Nat
    helper _ _ = x

-- 4. Punned promoted function vs forall shadow, and punned operator (+++):
punShadowedFun :: forall plus. plus -> foreach (m n :: Nat) ->
                  Proxy (data (plus m n)) -> Nat
punShadowedFun _ m n _ = plus m n

punOpType :: Int +++ Bool
punOpType = Left 42

punOpData :: foreach (xs ys :: List Bool) ->
             Proxy (data (xs +++ ys)) -> List Bool
punOpData xs ys _ = xs +++ ys

-- 5. Punned infix constructor (:+:) in type vs data mode:
infixType :: T :+: Bool
infixType = T :+: True

infixData :: Proxy (data (T :+: True))
infixData = Proxy

-- 6. Fallback from data mode to type namespace when unbound in data/var:
fallbackTy :: forall (k :: Nat). Vec Char (data (Succ k)) -> Proxy (data Nat)
fallbackTy _ = Proxy

-- 7. Promoted list and tuple constructors under data in NoListTuplePuns:
nilData :: Proxy (data ([] :: List T))
nilData = Proxy

singletonData :: Proxy (data [T])
singletonData = Proxy

twoElemData :: Proxy (data [T, T])
twoElemData = Proxy

unitData :: Proxy (data ())
unitData = Proxy

pairData :: Proxy (data (T, True))
pairData = Proxy

pairConData :: Proxy ((data (,)) (data T) (data False))
pairConData = Proxy :: Proxy (data ((,) T False))

-- 8. Visible @arg overrides using @(data ...) on invisible foreach and forall:
invisT :: foreach (t :: T). T
invisT @t = t

invisNat :: foreach (n :: Nat). Nat
invisNat @n = n

useAtDataT :: T
useAtDataT = invisT @(data T)

useAtDataVar :: foreach (x :: Nat) -> Nat
useAtDataVar x = check True
  where
    check :: forall x. x -> Nat
    check _ = invisNat @(data x)

-- 9. Required Type Arguments (T2T) with explicit type and data heralds:
idv :: forall (k :: Type) -> forall (a :: k) -> Proxy a
idv _ _ = Proxy

rtaPunTypeAndData :: Tup.Tuple2 (Proxy (T :: Type)) (Proxy (data T))
rtaPunTypeAndData = (idv Type (type T), idv (type T) (data T))

rtaNested :: Proxy (PairBox (data T))
rtaNested = idv Type (PairBox (data T))

idNatProxy :: forall (n :: Nat) -> Proxy n
idNatProxy _ = Proxy

rtaPunnedVar :: foreach (x :: Nat) -> Tup.Tuple2 (Proxy (data x)) Nat
rtaPunnedVar x = check (4 :: Int)
  where
    check :: forall x. x -> Tup.Tuple2 (Proxy (data x)) Nat
    check _ = (idNatProxy (data x), x)

-- 10. Term-level and pattern-level data herald identity:
termData :: T -> Bool
termData (data T) = data True

eqDataT :: Proxy (data T) :~: Proxy 'T
eqDataT = Refl

eqDataList :: Proxy (data [T]) :~: Proxy ['T]
eqDataList = Refl

eqDataPair :: Proxy (data (T, True)) :~: Proxy ('T, 'True)
eqDataPair = Refl

main :: IO Tup.Unit
main = do
  let v2 = vecTwo 'q'
      v3 = vecStep (Succ (Succ Zero)) 'r' v2
      len3 = punTermVar (Succ (Succ (Succ Zero))) v3
      sum2 = punShadowedFun ("shadow" :: List Char) (Succ Zero) (Succ Zero) Proxy
      appBools = punOpData [True] [False] Proxy
      atT = useAtDataT
      atVar = useAtDataVar (Succ (Succ Zero))
      (_, rtaX) = rtaPunnedVar (Succ Zero)
  seq eqDataT $
    seq eqDataList $
    seq eqDataPair $
    seq proxyTypeT $
    seq proxyDataT $
    seq proxyDataTrue $
    seq punOpType $
    seq infixType $
    seq infixData $
    seq (fallbackTy v3) $
    seq nilData $
    seq singletonData $
    seq twoElemData $
    seq unitData $
    seq pairData $
    seq pairConData $
    seq rtaPunTypeAndData $
    seq rtaNested $
    print (len3, sum2, appBools, atT, atVar, rtaX, termData T)
