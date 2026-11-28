{-# LANGUAGE DataKinds, ExistentialQuantification, NoDependentTypes, TypeFamilies #-}
module NativeDataFamilyDisabledNewtype where
import Data.Proxy (Proxy)
data family Wrap a
newtype instance Wrap [a] = MkWrap a
type Bad = Proxy ('MkWrap 'True)
