{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedFunctionRulesLaterBinder where
import Data.Kind (Type)

data Choice (b :: Bool) = Choice Int
{-# NOINLINE readKept #-}
readKept :: foreach (b :: Bool) -> Choice b -> Int
readKept _ (Choice n) = n
{-# RULES "later binder" forall x b. readKept b x = 0 #-}

data Reading (m :: Maybe a) = Reading Int
{-# NOINLINE keepReading #-}
keepReading :: forall (a :: Type). foreach (m :: Maybe a) -> Reading m -> Reading m
keepReading _ x = x
{-# RULES "later reading" forall x m. keepReading @Bool m x = x #-}
