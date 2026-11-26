{-# LANGUAGE GADTs, NoDependentTypes, OverloadedRecordDot #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Char (toUpper)
import NamedSiblingColumns
import qualified NamedSiblingColumns as Named
import NativeNamedSiblingHelpers
  ( runMissingChecks
  , runParamChecks
  , unrelatedGadtLength
  , unrelatedScopedLength
  )
import NativeNamedSiblingStrict (runStrictChecks)
import PackedVecLibrary (Nat(..), Vec(..), natToInt, previous, replicateNative, toList, zipVec)

readPairs :: NamedColumns -> [(Char, Bool)]
readPairs p = toList (zipVec (namedLetters p) (namedFlags p))

fillFromWitness :: NamedColumns -> [Char]
fillFromWitness p = toList (replicateNative (Named.storedLength p) '-')

fillFromPrevious :: NamedColumns -> [Char]
fillFromPrevious p = toList (replicateNative (previous (storedLength p)) '-')

readOnlyFlags :: NamedColumns -> [Bool]
readOnlyFlags NamedColumns{namedFlags = xs} = toList xs

mapVector :: (a -> b) -> Vec a n -> Vec b n
mapVector _ VNil = VNil
mapVector f (VCons x xs) = VCons (f x) (mapVector f xs)

negateFlags :: NamedColumns -> NamedColumns
negateFlags p = p { namedFlags = mapVector not (namedFlags p) }

updateBoth :: NamedColumns -> NamedColumns
updateBoth p = p { namedFlags = mapVector not (namedFlags p)
                 , namedLetters = mapVector toUpper (namedLetters p) }

updateOnlyDemanded :: NamedColumns -> NamedColumns
updateOnlyDemanded p = p { namedLetters = undefined
                         , namedFlags = mapVector not (namedFlags p) }

writeSameWitness :: NamedColumns -> NamedColumns
writeSameWitness p = p { Named.storedLength = Named.storedLength p
                       , namedFlags = mapVector not (namedFlags p) }

writeOnlyWitness :: NamedColumns -> NamedColumns
writeOnlyWitness p = p { storedLength = (storedLength (p)) }

omitFlags :: NamedColumns -> NamedColumns
omitFlags p = p { namedFlags = undefined }

emptyRecord :: NamedColumns -> NamedColumns
emptyRecord p = p { namedFlags = VNil, storedLength = Z, namedLetters = VNil }

main :: IO ()
main = do
  (unrelatedGadtLength, unrelatedScopedLength) `seq` pure ()
  let p = NamedColumns { namedFlags = VCons True VNil
                       , storedLength = S Z
                       , namedLetters = VCons 'r' VNil }
  print (readPairs p)
  let unusedPayloads = NamedColumns (S (S Z)) undefined undefined
  print (fillFromWitness unusedPayloads, fillFromPrevious unusedPayloads)
  print (natToInt ((NamedColumns Z undefined undefined).storedLength))
  print (readOnlyFlags (NamedColumns { namedFlags = VCons True VNil
                                     , storedLength = S Z
                                     , namedLetters = undefined }))
  let q = negateFlags p
  print (natToInt (storedLength q), readPairs q)
  print (readOnlyFlags (negateFlags (NamedColumns (S Z) undefined (VCons True VNil))))
  let r = omitFlags p
  print (toList (namedLetters r))
  let both = updateBoth p
  print (natToInt (storedLength both), readPairs both)
  print (readOnlyFlags (updateOnlyDemanded p))
  let same = writeSameWitness p
  print (natToInt (storedLength same), readPairs same)
  print (readPairs (writeOnlyWitness p))
  let lazyLength :: Nat
      lazyLength = undefined
  print (writeOnlyWitness (NamedColumns lazyLength undefined undefined) `seq` True)
  let empty = emptyRecord p
  print (natToInt (storedLength empty), readPairs empty)
  let noOldPayloads = emptyRecord (NamedColumns (S Z) undefined undefined)
  print (readPairs noOldPayloads)
  oldBottom <- try (evaluate (emptyRecord undefined)) :: IO (Either ErrorCall NamedColumns)
  print (either (const True) (const False) oldBottom)
  runParamChecks
  runMissingChecks
  runStrictChecks
