{-# LANGUAGE DependentTypes, ListTuplePuns, UnboxedSums, NoUnboxedTuples #-}
module NativeUnboxedTupleDisabled where
type P = '(# , #) 'True 'x'
