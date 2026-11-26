{-# LANGUAGE DataKinds, KindSignatures, NoDependentTypes, RankNTypes, TypeApplications, TypeOperators #-}
module Ordinary where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaScope
import TypeLambda (CheckedRow(..), Fields(..), L, Captured, Shadowed, WithError, Prefixes, ScopedRow, Mixed, Held, CaseShadow, Rebuild,
                   IndexedTag(..), IndexedCase, IndexedCaseInferred, rankedReader, rankedReaderAt)
import Data.Proxy (Proxy(..))
result :: L :~: '[Maybe Int, Either String Int, IO Int]
result = Refl
capture :: Captured Ordering :~: '[Either Ordering Bool, Either Ordering Char]
capture = Refl
readCells :: CheckedRow e '[Bool] -> Fields '[Either e Bool]
readCells = checkedCells
