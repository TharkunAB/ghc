{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, Modifiers, PolyKinds #-}
module NativeTypeLambdaBadRigidSkolemsU where
import Data.Kind (Type)
import GHC.Exts (U)
badRigidSkolems :: forall (p :: Type %U -> Type) (q :: Type %U -> Type) a. p a -> q a
badRigidSkolems x = x
