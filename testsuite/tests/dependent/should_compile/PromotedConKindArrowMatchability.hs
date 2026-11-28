{-# LANGUAGE DataKinds, GADTs, PatternSynonyms, TypeFamilies, TypeOperators, UnsaturatedTypeFamilies #-}
module PromotedConKindArrowMatchability where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))

infixr 5 :~
data Set a = Nil | a :~ Set a

type Singleton t = t ':~ 'Nil

type family Map (f :: Type -> Type) (ts :: Set Type) :: Set Type where
  _ `Map` 'Nil = 'Nil
  f `Map` (t ':~ ts) = f t ':~ (f `Map` ts)

data SetApp a = SetApp (Type -> Type) (Set a)

infixl 4 :$:
type f :$: ts = 'SetApp f (f `Map` ts)

data OneOrMore :: Set Type -> Type where
  Empty :: OneOrMore 'Nil
  (:.) :: Maybe a -> OneOrMore as -> OneOrMore (a ':~ as)

data OneOrMoreApp :: SetApp Type -> Type where
  OneOrMoreApp :: OneOrMore as -> OneOrMoreApp ('SetApp f as)

pattern SingletonApp :: a -> OneOrMoreApp ('SetApp f (Singleton a))
pattern SingletonApp x = OneOrMoreApp (Just x :. Empty)
{-# COMPLETE SingletonApp #-}

unSingleton :: OneOrMoreApp ('SetApp f (Singleton a)) -> a
unSingleton (SingletonApp x) = x

class Request e where
  data Occurred e

type EvOccs (es :: Set Type) = OneOrMoreApp (Occurred :$: es)

data Rct es r where
  Await :: e -> Rct (Singleton e) (EvOccs (Singleton e))

extractAwait :: Request e => Rct (Singleton e) (EvOccs (Singleton e)) -> (Occurred e -> r) -> OneOrMoreApp (Occurred :$: Singleton e) -> r
extractAwait _ k occs = k (unSingleton occs)

-- Verify that an open type family (`Unmatchable`) can also instantiate `'SetApp`.
type family OpenFam (a :: Type) :: Type

openFamSetApp :: Proxy ('SetApp OpenFam ('Nil :: Set Type))
openFamSetApp = Proxy
