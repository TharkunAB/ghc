{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
module NativeRetainedConstructorContextWrong where
import Data.Type.Equality ((:~:)(Refl))
import Provider (T(..))

wrong :: T a -> a :~: Bool
wrong (Mk _ _) = Refl
