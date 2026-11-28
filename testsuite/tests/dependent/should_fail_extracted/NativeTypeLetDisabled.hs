{-# LANGUAGE NoDependentTypes #-}
module NativeTypeLetDisabled where
type Bad = let b = Bool in Maybe b
type BadGroup = let { a = Bool; b = Maybe a } in b
