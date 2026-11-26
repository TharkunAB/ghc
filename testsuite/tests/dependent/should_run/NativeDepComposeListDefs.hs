{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}

module NativeDepComposeListDefs (Nat (..), plus, foldr) where

import Prelude hiding (foldr)

data Nat = Zero | Succ Nat

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ z []       = z
foldr f z (x : xs) = f x (foldr f z xs)
