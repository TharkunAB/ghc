{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies, UndecidableInstances #-}
module NativeTypeCaseRecursiveIndex where
import Data.Kind (Type)

type family Loop (tag :: Bool) :: Type where
  Loop 'True = Loop 'True
  Loop 'False = Ordering

data Gate k (tag :: Bool) where
  Known :: Bool -> Gate Bool 'True
  Recursive :: Gate (Loop tag) tag

type Pick (d :: Maybe (Gate Bool 'True)) = case d of
  Just (Known True) -> Int
  Just _ -> Bool
  Nothing -> ()

type Overflow (d :: Maybe (Gate Bool 'True)) = case d of
  Just Recursive -> Int
  Just _ -> Bool
  Nothing -> ()
