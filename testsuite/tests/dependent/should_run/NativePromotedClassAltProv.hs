{-# OPTIONS_GHC -O2 #-}
module NativePromotedClassAltProv (Alternate(..)) where
class Alternate t where
    spare :: t a -> ()
    alternate :: (a -> b) -> t a -> t b
instance Alternate Maybe where
    spare _ = ()
    alternate _ _ = Nothing
