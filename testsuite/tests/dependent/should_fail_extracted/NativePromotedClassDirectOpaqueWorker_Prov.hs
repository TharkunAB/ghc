module NativePromotedClassDirectOpaqueWorker_Prov (Alternate(..)) where
class Alternate t where
    alternate :: (a -> b) -> t a -> t b
instance Alternate Maybe where
    {-# OPAQUE alternate #-}
    alternate _ _ = Nothing
