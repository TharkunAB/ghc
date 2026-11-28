{-# LANGUAGE NoDependentTypes, TypeApplications #-}
module NativeIndexedFinWrongElemTy where
import AlignedList (zipWithExact)
import PackedVecLibrary (toList)

wrongElemTy :: [Int]
wrongElemTy = toList (zipWithExact @Char @Int @Int [5 + 1, 'x'] [10,20] (\_ y -> y))
