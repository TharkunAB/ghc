{-# LANGUAGE NoDependentTypes #-}
module NativeParamRecUpdateWrongReceiver where
import NativeParamDependentRecordUpdateProvider
bad :: Columns a -> Columns a -> Columns a
bad p q = p { cells = cells q }
