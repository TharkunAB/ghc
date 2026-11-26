{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeFamilies #-}
module NativeDataFamilyOperandStrictField where
import TupleProvider (At(..))
data family StrictTuple a
data instance StrictTuple Bool = StrictTuple !Bool
keepStrict :: foreach (t :: StrictTuple Bool) -> At t
keepStrict _ = At
strict :: At ('StrictTuple 'True)
strict = keepStrict (StrictTuple True)
