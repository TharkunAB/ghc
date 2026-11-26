{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns, TypeFamilies #-}
module NativeDataFamilyConstrainedCon where
import Data.Proxy (Proxy)
class Pick a
instance Pick Bool
data family Stored a
data instance Stored a where
  MkStored :: Pick a => a -> Stored a
type Bad = Proxy ('MkStored 'True)
