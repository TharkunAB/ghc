{-# LANGUAGE NoDependentTypes #-}
module NativeParamRecUpdateInconsistent where
import NativeParamDependentRecordUpdateProvider
bad :: SameCols Char -> SameCols Bool
bad s = s { firstCells = mapVec (== 'a') (firstCells s)
          , secondCells = mapVec (\_ -> (0 :: Int)) (secondCells s) }
