{-# LANGUAGE NoDependentTypes, ScopedTypeVariables #-}
module NativePromotedClassMissingGiven where
class Choose a where
    choose :: a -> Bool
chooseBoth :: forall a b. Choose a => a -> b -> (Bool, Bool)
chooseBoth x y = (choose x, choose y)
