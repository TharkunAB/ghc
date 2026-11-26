{-# LANGUAGE GADTs, NoDependentTypes, TypeFamilies #-}
module NativeOpenMatchabilityParent where
import NativeOpenMatchabilityFixed (Varianced)

data family Parser :: Varianced
data instance Parser = P

data Other = Other
data Ordinary a where
  Wrong :: Other

data family FamParser a
data instance FamParser Bool where
  WrongFam :: Other
