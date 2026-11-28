{-# LANGUAGE GHC2021, DataKinds, NoDependentTypes, TypeFamilies #-}
module OpenFamBinderKindMatchability where

import Data.Kind (Type)

type Effect = (Type -> Type) -> Type -> Type

data Dispatch = Dynamic | Static

type family DispatchOf (e :: Effect) :: Dispatch
type family EffectRep (d :: Dispatch) :: Effect -> Type
data family StaticRep (e :: Effect) :: Type
type instance EffectRep 'Static = StaticRep

data Env (es :: [Effect]) = Env

fromAnyEffect :: () -> EffectRep (DispatchOf e) e
fromAnyEffect _ = error "unused"

getEnv :: forall (e :: Effect) es. Env (e : es) -> EffectRep (DispatchOf e) e
getEnv _ = fromAnyEffect ()

data Reader (r :: Type) (m :: Type -> Type) (a :: Type)
type instance DispatchOf (Reader r) = 'Static
newtype instance StaticRep (Reader r) = ReaderRep r

runStatic :: (DispatchOf e ~ 'Static) => StaticRep e -> Env (e : es) -> StaticRep e
runStatic rep _ = rep

getReader :: Env (Reader Int : es) -> StaticRep (Reader Int)
getReader env = getEnv env
