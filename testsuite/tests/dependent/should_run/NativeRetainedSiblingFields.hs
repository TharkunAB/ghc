{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeOperators #-}
module Main (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..), natToInt, toList)
import SiblingColumnRow (packRow)
import SiblingColumns (Columns(..), bottomFields, flags, letters, size)
import SiblingColumnsClient (readPairs)
import TypeLambda (Fields(..))

knownLength :: size ('Columns 'Z 'VNil 'VNil) :~: 'Z
knownLength = Refl

readLetters :: Columns -> String
readLetters p = toList (letters p)

readFlags :: Columns -> [Bool]
readFlags p = toList (flags p)

readRow :: Columns -> (String, [Bool])
readRow p = unpack (packRow p)
  where
    unpack :: Fields '[Vec Char n, Vec Bool n] -> (String, [Bool])
    unpack (Field xs (Field ys End)) = (toList xs, toList ys)

throws :: String -> a -> IO Bool
throws label value = do
  result <- try (evaluate (value `seq` ())) :: IO (Either ErrorCall ())
  pure (either (isPrefixOf label . displayException) (const False) result)

main :: IO ()
main = case knownLength of
  Refl -> do
    ls <- throws "forced letters" (readLetters bottomFields)
    bs <- throws "forced flags" (readFlags bottomFields)
    print (natToInt (size (Columns (S Z) undefined undefined)),
           readLetters (Columns (S Z) (VCons 'r' VNil) undefined),
           readFlags (Columns (S Z) undefined (VCons True VNil)),
           ls, bs)
    let columns = Columns (S (S Z))
            (VCons 'r' (VCons 's' VNil))
            (VCons True (VCons False VNil))
    print (readPairs columns)
    print (readRow columns)
