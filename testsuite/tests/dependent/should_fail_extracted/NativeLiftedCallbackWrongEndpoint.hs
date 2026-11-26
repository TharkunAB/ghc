{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeLiftedCallbackWrongEndpoint where
import Data.Kind (Type)
import NativeLiftedCallbackProvider (Decision, decide)

wrongHead :: forall (a :: Type). foreach (f :: a -> Bool) -> foreach (g :: a -> Bool)
          -> foreach (x :: a) -> Decision (f x)
wrongHead _ g x = decide @a g x

wrongArg :: forall (a :: Type). foreach (f :: a -> Bool) -> foreach (x :: a)
         -> foreach (y :: a) -> Decision (f x)
wrongArg f _ y = decide @a f y
