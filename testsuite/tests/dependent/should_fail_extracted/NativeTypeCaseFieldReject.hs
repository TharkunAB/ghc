{-# LANGUAGE DataKinds, GADTs, LinearTypes, NoDependentTypes, PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications #-}
module NativeTypeCaseFieldReject where
import Data.Proxy (Proxy(..))
import NativeNestedCase
import NativeTypeCaseFieldCompoundKind
import NativeTypeCaseRetainedField

rigid :: CaseField ('Just bit) -> Int
rigid (CaseField n) = n

shadow :: ShadowedCaseField 'True ('Just 'False) -> Proxy 'True
shadow (ShadowedCaseField bit) = bit

ranked :: forall (bit :: Bool). Proxy bit -> OrderedRankedCaseField 'True -> Int
ranked _ (OrderedRankedCaseField f) = f @('Just bit) Proxy

rankedShadow :: RankedCaseField 'False -> Char
rankedShadow (RankedCaseField f) = f @'True Proxy

badCapture :: CapturedKindCaseField Bool 'True 'False -> Proxy Int
badCapture (CapturedKindCaseField f) = f @Int @'True Proxy

swapped :: forall k l. Compound k l ('Tag :: Tag k l) -> Proxy ('Tag :: Tag l k)
swapped (Compound f) = f @Int @Double @'True Proxy

dropInput :: LinearArrowDomain 'True
dropInput _ = 0

duplicateInput :: LinearArrowDomain 'True
duplicateInput x = x + x

dropPolyInput :: MultiplicityArrowDomain 'True
dropPolyInput _ = Proxy

wrongRetained :: RetainedField -> Char
wrongRetained (RetainedField f) = f True

neutralRetained :: Bool -> RetainedField -> Int
neutralRetained b (RetainedField f) = f b

unrelatedRetained :: Bool -> Bool -> RetainedField -> String
unrelatedRetained b other (RetainedField f) = case b of
  True -> show (f other :: Int)
  False -> [f b]

guardedRetained :: Bool -> Bool -> RetainedField -> String
guardedRetained b enabled (RetainedField f) = case b of
  True | enabled -> show (f b :: Int)
  True -> "fallthrough"
  False -> [f False]
