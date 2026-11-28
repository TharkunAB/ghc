{-# LANGUAGE CPP, DataKinds, DependentTypes, ListTuplePuns, GADTs, ScopedTypeVariables #-}
module Provider
    ( Choose(..), observe, observeBoth, observeGround, ignorePayload, opposite, lazyControls
#ifdef SCOPED_DICTIONARY_EXPORT
    , Tagged(..), chooseTagged, chooseTaggedBoth
#endif
    ) where

class Choose a where
    choose :: a -> Bool

instance Choose Bool where
    choose x = x

data Tagged (b :: Bool) where
    Tagged :: Bool -> Tagged b

tagBool :: foreach (b :: Bool) -> Tagged b
tagBool b = Tagged @b b

chooseTaggedBoth :: forall a b. (Choose b, Choose a)
                 => foreach (x :: a) -> foreach (y :: b)
                 -> (Tagged (choose x), Tagged (choose y))
chooseTaggedBoth x y = (tagBool (choose x), tagBool (choose y))

chooseTagged :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
chooseTagged x = tagBool (choose x)

forward :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
forward x = chooseTagged x

forwardBoth :: forall a b. (Choose a, Choose b)
            => foreach (x :: a) -> foreach (y :: b)
            -> (Tagged (choose x), Tagged (choose y))
forwardBoth x y = chooseTaggedBoth x y

observe :: Choose a => a -> Bool
observe x = case forward x of
    Tagged value -> value

observeBoth :: (Choose a, Choose b) => a -> b -> (Bool, Bool)
observeBoth x y = case forwardBoth x y of
    (Tagged lhs, Tagged rhs) -> (lhs, rhs)

observeGround :: Bool -> Bool
observeGround x = case chooseTagged x of
    Tagged value -> value

ignorePayload :: Choose a => a -> ()
ignorePayload x = case forward x of
    Tagged _ -> ()

data Inverted = Inverted Bool
instance Choose Inverted where
    choose (Inverted b) = not b

data Ignored = Ignored
instance Choose Ignored where
    choose _ = True

data Unobserved = Unobserved
instance Choose Unobserved where
    choose _ = error "unused method"

opposite :: Bool -> (Bool, Bool)
opposite b = observeBoth b (Inverted b)

lazyControls :: (Bool, ())
lazyControls = (observe (error "unused operand" :: Ignored), ignorePayload Unobserved)
