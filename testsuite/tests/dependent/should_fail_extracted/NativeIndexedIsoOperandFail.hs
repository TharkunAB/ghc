{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeIndexedIsoOperandFail where

data Proof (index :: Bool) where
    Base :: Proof 'False

data Tag (b :: Bool) where
    Tag :: Tag b

{-# NOINLINE tag #-}
tag :: foreach (b :: Bool) -> Tag b
tag _ = Tag

wrong :: foreach (encode :: Proof 'False -> Bool) -> Tag 'True
wrong encode = tag (encode Base)
