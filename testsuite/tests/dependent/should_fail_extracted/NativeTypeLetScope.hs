{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeLetScope where
type Recursive = let b = b in Maybe b
type Forward = let { a = b; b = Bool } in Maybe a
type SignedForward = let { width :: later; width = 'True; later = Bool } in width
