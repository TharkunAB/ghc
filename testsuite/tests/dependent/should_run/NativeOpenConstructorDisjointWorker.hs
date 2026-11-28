{-# LANGUAGE NoDependentTypes #-}
{-# OPTIONS_GHC -fno-unoptimized-core-for-interpreter -fno-omit-interface-pragmas -fexpose-all-unfoldings #-}
module NativeOpenConstructorDisjointWorker
  ( Choice(..), Nested(..), pick, route, pickNested ) where

data Choice a b = LeftFields a b | RightFields b a
data Nested a = Nested (Maybe a)

{-# INLINE pick #-}
pick :: Choice a b -> b
pick (LeftFields _ y) = y
pick (RightFields y _) = y

{-# INLINE route #-}
route :: Choice a b -> b
route fields = pick fields

{-# INLINE pickNested #-}
pickNested :: Nested a -> a -> a
pickNested (Nested (Just x)) _ = x
pickNested (Nested Nothing) x = x
