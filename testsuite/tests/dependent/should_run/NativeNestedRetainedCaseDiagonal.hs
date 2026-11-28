{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeNestedRetainedCaseDiagonal where
import Data.Type.Equality ((:~:) (Refl))

{-# INLINE check #-}
check :: forall a (x :: a). foreach (p :: x :~: x) -> p :~: 'Refl
check p = case p of Refl -> Refl
