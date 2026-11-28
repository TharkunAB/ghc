{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeApplications,
             TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:)(Refl))
import NativeLocalListAccumulatorLib (shunt)
import NativeLocalListAccumulatorRevProof (revStep)

open :: foreach (xs :: [Bool]) -> foreach (ys :: [Bool])
     -> shunt ('True ': xs) ys :~: shunt xs ('True ': ys)
open _ _ = Refl

onEmpty :: foreach (ys :: [Bool]) -> shunt '[] ys :~: ys
onEmpty _ = Refl

{-# NOINLINE spine #-}
spine :: [Bool] -> Bool
spine [] = False
spine (_ : _) = True

main :: IO ()
main = case revStep True [] of
  Refl -> do
    print (open [] [], onEmpty [])
    print (shunt [True, False] [True])
    print (spine (shunt [error "head forced"] (error "accumulator forced")))
    demanded <- try @ErrorCall (evaluate (spine (shunt (True : error "input tail") [])))
    case demanded of
      Left exception | "input tail" `isInfixOf` displayException exception ->
        putStrLn "demanded input tail"
      _ -> fail "skipped input tail"
