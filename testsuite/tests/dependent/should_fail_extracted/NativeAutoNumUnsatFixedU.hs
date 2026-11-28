{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds, UnsaturatedTypeFamilies #-}
module NativeAutoNumUnsatFixedU where
import Data.Kind (Type)
type UEndo = Type -> Type
data BadTag :: UEndo where
  MkBadTag :: BadTag Int
