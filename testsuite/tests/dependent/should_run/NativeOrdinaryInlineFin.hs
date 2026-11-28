{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeOrdinaryInlineFin (zero, deferred) where
import FinitePosition
import GHC.Exts (inline)
import PackedVecLibrary (Nat(..))
import ParameterizedIndexLibrary (Fin(..))

{-# NOINLINE inspect #-}
inspect :: forall n. foreach (i :: Fin n) -> FinView i
inspect i = viewFin i

{-# INLINE zero #-}
zero :: forall n. FinView ('FZ @n)
zero = inspect @('S n) FZ

{-# INLINE deferred #-}
deferred :: Applicative f => f (FinView ('FZ @'Z))
deferred = pure (inline (zero @'Z))
