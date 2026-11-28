{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, RankNTypes,
             ScopedTypeVariables, TypeFamilies, TypeOperators #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (build)
import MapRuleReplacement (replacement)
import NativeMapWitnessLib (NegateAll, First(..), Checked(..), checked)

type family Plain a :: Bool where
  Plain Int = 'False

readIndex :: Proxy xs -> ()
readIndex _ = ()

ordinary :: ((), Plain Int :~: 'False)
ordinary = (readIndex (Proxy :: Proxy (Plain Int)), Refl)

firstFromProvider :: First (NegateAll '[ 'True])
firstFromProvider = checkedFirst checked

checkedFirst :: Checked xs -> First xs
checkedFirst (Checked Refl h) = h

headValue :: First xs -> Bool
headValue FalseHead = False
headValue TrueHead  = True

readChecked :: forall xs. Checked xs -> Bool
readChecked witness = headValue (checkedFirst witness)

firstChecked :: [Checked xs] -> Maybe Bool
firstChecked [] = Nothing
firstChecked (w : _) = Just (readChecked w)

{-# RULES "replace introduced Bool builder" [~1]
    forall (g :: forall r. (Bool -> r -> r) -> r -> r). build g = replacement #-}

{-# NOINLINE runtimeHead #-}
runtimeHead :: [Bool] -> Bool
runtimeHead xs = case map not (True : xs) of
  x : _ -> x
  [] -> error "expected the replacement list"

main :: IO ()
main = case ordinary of
  ((), Refl) ->
    print (headValue firstFromProvider, fmap readChecked (Just checked),
           firstChecked [checked], runtimeHead [])
