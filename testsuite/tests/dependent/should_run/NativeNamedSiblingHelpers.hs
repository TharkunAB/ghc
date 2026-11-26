{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module NativeNamedSiblingHelpers
  ( checked
  , fillAtWitness
  , replaceFlags
  , replaceAll
  , updateWithProof
  , unrelatedGadtLength
  , unrelatedScopedLength
  , paramPayload
  , runParamChecks
  , runMissingChecks
  , onlyFlagsAtVar
  ) where

import Control.Exception (RecConError(..), evaluate, try)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import NamedSiblingColumns (NamedColumns(..))
import qualified NamedSiblingColumns as NS
import PackedVecLibrary (Nat(..), Vec(..), natToInt, replicateNative, toList, zipVec)
import qualified ParamColumns as PC

checked :: foreach (p :: NamedColumns) -> Vec Char (NS.storedLength p)
checked p = namedLetters p

fillAtWitness :: foreach (p :: NamedColumns) -> Vec Char (NS.storedLength p)
fillAtWitness p = replicateNative (NS.storedLength p) '-'

replaceFlags :: foreach (p :: NamedColumns) -> Vec Bool (NS.storedLength p) -> NamedColumns
replaceFlags p flags = p { namedFlags = flags }

replaceAll :: NamedColumns -> foreach (n :: Nat) -> Vec Char n -> Vec Bool n -> NamedColumns
replaceAll p n letters flags = p { NS.storedLength = n, namedLetters = letters, namedFlags = flags }

updateWithProof :: foreach (p :: NamedColumns) (q :: NamedColumns)
                -> (NS.storedLength p :~: NS.storedLength q) -> NamedColumns
updateWithProof p q Refl = p { NS.storedLength = NS.storedLength q, namedFlags = namedFlags q }

data GadtColumns where
  GadtColumns :: { gLength :: Nat, gLetters :: Vec Char gLength, gFlags :: Vec Bool gLength } -> GadtColumns

data Scoped (sLength :: Nat) = Scoped
  { sLength :: Nat, sLetters :: Vec Char sLength, sFlags :: Vec Bool sLength }

unrelatedGadtLength :: GadtColumns
unrelatedGadtLength = GadtColumns (S Z) VNil VNil

unrelatedScopedLength :: Scoped 'Z
unrelatedScopedLength = Scoped (S Z) VNil VNil

paramPayload :: forall a. foreach (r :: PC.Columns a) -> Vec a (PC.storedLength r)
paramPayload r = PC.cells r

runParamChecks :: IO ()
runParamChecks = do
  let payload r = let vec = PC.cells r in toList vec
      direct r = toList (PC.cells r)
      beside r = toList (zipVec (PC.cells @Char r) (replicateNative (PC.storedLength r) '-'))
      made :: PC.Columns Char
      made = PC.Columns { PC.cells = VCons 'a' VNil, PC.storedLength = S Z }
  print (payload made, direct made, beside made)
  print (natToInt (PC.storedLength @Bool (PC.Columns Z undefined)))

onlyFlagsAtVar :: foreach (n :: Nat) -> Vec Bool n -> NamedColumns
onlyFlagsAtVar n flags = NamedColumns { NS.storedLength = n, namedFlags = flags }

runMissingChecks :: IO ()
runMissingChecks = do
  let letters p = toList (namedLetters p)
      flags p = toList (namedFlags p)
      missingOne = NamedColumns { NS.storedLength = S Z, namedFlags = VCons True VNil }
      missingBoth = NamedColumns { NS.storedLength = S Z }
      checkMissing field value = do
        result <- try (evaluate value >> pure ())
        case result of
          Left (RecConError msg) | field `isInfixOf` msg -> putStrLn field
          _ -> fail ("expected the missing-field exception for " ++ field)
  print (flags missingOne)
  checkMissing "namedLetters" (letters missingOne)
  print (natToInt (NS.storedLength missingBoth))
  checkMissing "namedLetters" (letters missingBoth)
  checkMissing "namedFlags" (flags missingBoth)
