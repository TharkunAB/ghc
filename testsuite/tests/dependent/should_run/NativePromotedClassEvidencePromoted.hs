{-# LANGUAGE BangPatterns, DataKinds, DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables #-}
module NativePromotedClassEvidencePromoted (chooseTaggedBothP, concreteP) where

class ChooseP a where
    chooseP :: a -> Bool

instance ChooseP Bool where
    chooseP x = x

data TaggedP (b :: Bool) where
    TaggedTrueP :: TaggedP 'True
    TaggedFalseP :: TaggedP 'False

tagBoolP :: foreach (b :: Bool) -> TaggedP b
tagBoolP True = TaggedTrueP
tagBoolP False = TaggedFalseP

chooseTaggedBothP :: forall a b. (ChooseP b, ChooseP a)
                  => foreach (x :: a) -> foreach (y :: b)
                  -> (TaggedP (chooseP x), TaggedP (chooseP y))
chooseTaggedBothP x y = (tagBoolP (chooseP x), tagBoolP (chooseP y))

chooseTaggedP :: forall a. ChooseP a => foreach (x :: a) -> TaggedP (chooseP x)
chooseTaggedP x = tagBoolP (chooseP x)

concreteP :: Bool -> ()
concreteP x = case chooseTaggedP x of !_ -> ()
