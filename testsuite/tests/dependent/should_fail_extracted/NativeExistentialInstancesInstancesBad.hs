{-# LANGUAGE DependentTypes, ListTuplePuns, FlexibleContexts, FlexibleInstances #-}
module NativeExistentialInstancesInstancesBad where
import ErasedStream (Stream)

class Sample a
instance Sample (a,a) => Sample (Maybe (Stream a))

class Dup a
type First = exists i. (i, i -> Bool)
type Second = exists j. (j, j -> Bool)
instance Dup First
instance Dup Second
