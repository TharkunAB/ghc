{-# LANGUAGE NoDependentTypes #-}
module NativeExtendedLetDisabled where
bad :: Int
bad = let type T = Int in (1 :: T)
