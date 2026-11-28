{-# LANGUAGE DependentTypes, TypeApplications #-}
module NativeProposal378BadInferredConField where
import NativeProposal378PatternsAndT2TProvider
bad :: InvisVec Bool
bad = MkInvisInf @Bool @Zero VNil
