{-# LANGUAGE NoDependentTypes #-}
module NativeParamRecUpdateMissingPayload where
import NativeParamDependentRecordUpdateProvider
bad :: Columns a -> Columns a
bad r = r { storedLength = Z }
