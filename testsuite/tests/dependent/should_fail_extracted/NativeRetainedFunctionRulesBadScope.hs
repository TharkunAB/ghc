{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedFunctionRulesBadScope where
import Data.Kind (Type)

data Choice (b :: Bool) = Choice Int
{-# NOINLINE compareAt #-}
compareAt :: foreach (b :: Bool) -> foreach (c :: Bool) -> Choice b -> Choice c -> Int
compareAt _ _ _ _ = 0
{-# RULES "different indices" forall b c x. compareAt b c x x = 0 #-}

data Reading (m :: Maybe a) = Reading Int
{-# NOINLINE keepReading #-}
keepReading :: forall (a :: Type). foreach (m :: Maybe a) -> Reading m -> Reading m
keepReading _ x = x
{-# RULES "wrong kind reading" forall m x. keepReading @Maybe m x = x #-}

{-# NOINLINE keepAfter #-}
keepAfter :: Bool -> forall (a :: Type). foreach (m :: Maybe a) -> Reading m -> Reading m
keepAfter _ @a = keepReading @a
{-# RULES "wrong kind after" forall ignored m x. keepAfter ignored @Maybe m x = x #-}

keepTwo :: forall a. foreach (m :: Maybe a) -> Maybe a -> Reading m -> Reading m
keepTwo _ _ x = x
{-# NOINLINE keepAfterTwo #-}
keepAfterTwo :: Bool -> forall a. foreach (m :: Maybe a) -> Maybe a -> Reading m -> Reading m
keepAfterTwo _ @a = keepTwo @a
{-# RULES "independent" forall b m (other :: Maybe Bool) (x :: Reading other).
    keepAfterTwo b @Bool m other x = x #-}

data TaggedReading (tag :: Bool) (m :: Maybe a) = TaggedReading Int
{-# NOINLINE keepRequired #-}
keepRequired :: foreach (tag :: Bool) -> forall (a :: Type) ->
                foreach (m :: Maybe a) -> TaggedReading tag m -> TaggedReading tag m
keepRequired _ (type _) _ x = x
{-# NOINLINE independentReq #-}
independentReq :: foreach (left :: Bool) -> Bool -> forall (a :: Type) ->
                  foreach (m :: Maybe a) -> TaggedReading left m -> TaggedReading left m
independentReq left _ (type a) m x = keepRequired left (type a) m x
{-# RULES "independentReq" forall left (right :: Bool) m x.
    independentReq left right (type Bool) m x = keepRequired right (type Bool) m x #-}
