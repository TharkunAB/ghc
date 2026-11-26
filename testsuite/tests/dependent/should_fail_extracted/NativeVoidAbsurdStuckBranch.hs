{-# LANGUAGE DependentTypes #-}
module NativeVoidAbsurdStuckBranch where
import Data.Type.Equality ((:~:) (Refl))
import Data.Void (Void)
okTo :: Either a Void -> a
okTo (Left x)  = x
okTo (Right v) = case v of {}
unsound :: foreach (v :: Void) -> foreach (x :: a) -> okTo (Right v) :~: x
unsound _ _ = Refl
