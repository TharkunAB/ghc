{-# LANGUAGE NoDependentTypes #-}
module NativeTypeCaseScopedResultReject where
import NativeTypeCaseScopedResultDomain (Choice)
import qualified NativeTypeCaseScopedResultProvide as P
import qualified NativeTypeCaseScopedResultConsume as C

badField :: Choice -> String
badField d = C.otherField d (P.field d)

badCapture :: Bool -> Bool -> Choice -> String
badCapture a b d = C.otherCapture a b d (P.capture a b d)

badCaptureArg :: Bool -> Bool -> Choice -> String
badCaptureArg a b d = C.sameCapture a b d (P.capture b a d)

badIndex :: Choice -> Choice -> String
badIndex d e = C.sameField d (P.field e)

badNestedIndex :: Bool -> Choice -> Choice -> String
badNestedIndex a d e = C.nested a d (P.nested a e)

badNestedResult :: Bool -> Choice -> String
badNestedResult a d = C.nestedOther a d (P.nested a d)
