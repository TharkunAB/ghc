{-# LANGUAGE DataKinds, KindSignatures, NoDependentTypes, RankNTypes, TypeApplications, TypeOperators #-}
module NativeTypeLambdaOrdinary where
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaScope
import TypeLambda (Captured, Prefixes, ScopedRow, Fields, rankedReader, rankedReaderAt)

capture :: Captured Ordering :~: '[Either Ordering Bool, Either Ordering Char]
capture = Refl

capturedRow :: Prefixes ('Just Ordering) '[Char, Bool] :~: '[Either Ordering Char, Either Ordering Bool]
capturedRow = Refl

independentRow :: Prefixes 'Nothing '[Char, Bool] :~: '[Maybe Char, Maybe Bool]
independentRow = Refl

scopedRow :: ScopedRow Char '[Bool] :~: '[(Maybe Char, Either Char Bool)]
scopedRow = Refl

rankedSynonym :: forall (column :: Bool). Proxy column -> Fields '[ (Proxy 'True, Proxy column, Char) ]
rankedSynonym = rankedReader

rankedSignature :: forall (column :: Bool). Proxy column -> Fields '[ (Proxy 'True, Proxy column, Char) ]
rankedSignature = rankedReaderAt (Proxy :: Proxy 'True)
