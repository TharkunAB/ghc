{-# LANGUAGE NoDependentTypes, Strict #-}
module NativeNamedSiblingStrict (runStrictChecks) where

import Control.Exception (ErrorCall, evaluate, try)
import NamedSiblingColumns
import PackedVecLibrary (Nat(..), Vec(..))

clear :: NamedColumns -> NamedColumns
clear p = p { storedLength = Z, namedLetters = undefined, namedFlags = VNil }

writeWitness :: NamedColumns -> NamedColumns
writeWitness p = p { storedLength = storedLength p }

data Ordinary = Ordinary { field :: ~Bool }

update :: Ordinary -> Ordinary
update p = p { field = undefined }

checkDemand :: a -> IO ()
checkDemand ~value = do
  forced <- try (evaluate value >> pure ()) :: IO (Either ErrorCall ())
  print (either (const True) (const False) forced)

runStrictChecks :: IO ()
runStrictChecks = do
  checkDemand (clear (NamedColumns Z VNil VNil))
  checkDemand (update (Ordinary False))
  checkDemand (writeWitness (NamedColumns Z undefined undefined))
  let missingLength :: Nat
      ~missingLength = undefined
  checkDemand (writeWitness (NamedColumns missingLength undefined undefined))
