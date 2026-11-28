{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeExistentialSourceNamesake (normal) where
data Wrap = Pack Int
normal :: Int
normal = case Pack 7 of Pack value -> value
