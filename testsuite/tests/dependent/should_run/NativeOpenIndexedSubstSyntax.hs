{-# LANGUAGE DataKinds, GADTs, LambdaCase, NoDependentTypes, RankNTypes #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas -fexpose-all-unfoldings -fno-write-source-equations #-}
module NativeOpenIndexedSubstSyntax where

import Data.Type.Equality ((:~:)(Refl))

data Sort = Num | Fun Sort Sort
data Ctx = Empty | Extend Ctx Sort

data Name (g :: Ctx) (a :: Sort) where
  Here :: Name ('Extend g a) a
  There :: Name g a -> Name ('Extend g b) a

data Term (g :: Ctx) (a :: Sort) where
  Use :: Name g a -> Term g a
  Bind :: Term ('Extend g a) b -> Term g ('Fun a b)
  Lit :: Term g 'Num

liftRename :: (forall a. Name g a -> Name d a)
           -> Name ('Extend g b) a -> Name ('Extend d b) a
liftRename _ Here = Here
liftRename rename (There x) = There (rename x)

renameTerm :: (forall a. Name g a -> Name d a) -> Term g b -> Term d b
renameTerm rename (Use x) = Use (rename x)
renameTerm rename (Bind body) = Bind (renameTerm (liftRename rename) body)
renameTerm _ Lit = Lit

liftSubst :: (forall a. Name g a -> Term d a)
          -> Name ('Extend g b) a -> Term ('Extend d b) a
liftSubst _ Here = Use Here
liftSubst sub (There x) = renameTerm There (sub x)

substTerm :: (forall a. Name g a -> Term d a) -> Term g b -> Term d b
substTerm sub (Use x) = sub x
substTerm sub (Bind body) = Bind (substTerm (liftSubst sub) body)
substTerm _ Lit = Lit

substOne :: Term ('Extend g a) b -> Term g a -> Term g b
substOne body value = substTerm (\case Here -> value; There x -> Use x) body

data SomeTerm (g :: Ctx) (a :: Sort) where
  Pack :: (x :~: a) -> Term g x -> SomeTerm g a

packTerm :: Name g a -> Term g a -> SomeTerm g a
packTerm _ term = Pack Refl term

unpackTerm :: Name g a -> SomeTerm g a -> Term g a
unpackTerm _ (Pack Refl term) = term

unpackOne :: SomeTerm g a -> Term g a
unpackOne (Pack Refl term) = term
