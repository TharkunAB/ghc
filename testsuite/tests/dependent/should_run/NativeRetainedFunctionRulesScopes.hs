{-# LANGUAGE AllowAmbiguousTypes, DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeRetainedFunctionRulesScopes where
import Data.Kind (Type)
import Data.Proxy (Proxy)

data Reading (m :: Maybe a) (b :: Bool) = Reading Int
{-# NOINLINE keepReading #-}
keepReading :: forall (a :: Type). foreach (m :: Maybe a) -> foreach (b :: Bool)
            -> Reading m b -> Reading m b
keepReading _ _ x = x
{-# RULES "keep/reading" forall (m :: Maybe a) b x. keepReading m b x = x #-}

type family Payload a c (b :: Bool) :: Type
{-# NOINLINE keepAt #-}
keepAt :: forall (a :: Type) (c :: Type). foreach (b :: Bool) -> Payload a c b -> Payload a c b
keepAt _ x = x
{-# RULES "scoped" forall (b :: Bool) x. keepAt @(Proxy b) @Char b x = x #-}
{-# NOINLINE keepBetween #-}
keepBetween :: forall a. Proxy a -> forall c. foreach (b :: Bool) -> Payload a c b -> Payload a c b
keepBetween _ @c = keepAt @a @c
{-# RULES "between" forall p b x. keepBetween @Int p @Char b x = x #-}

data OneReading (m :: Maybe a) = OneReading Int
keepTwo :: forall a. foreach (m :: Maybe a) -> Maybe a -> OneReading m -> OneReading m
keepTwo _ _ x = x
{-# NOINLINE keepAfter #-}
keepAfter :: Bool -> forall a. foreach (m :: Maybe a) -> Maybe a -> OneReading m -> OneReading m
keepAfter _ @a = keepTwo @a
{-# RULES "sameIndex" forall b m (other :: Maybe Bool) (x :: OneReading m).
    keepAfter b @Bool m other x = x #-}
