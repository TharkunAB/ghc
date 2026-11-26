{-# LANGUAGE NoDependentTypes #-}
module NativeParamRecUpdatePinnedMismatch where
import NativeParamDependentRecordUpdateProvider
bad :: SameCols Char -> SameCols Bool
bad s = s { firstCells = mapVec (== 'a') (firstCells s) }
