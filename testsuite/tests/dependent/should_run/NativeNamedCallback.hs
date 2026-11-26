{-# LANGUAGE GADTs, NoDependentTypes, TypeApplications #-}
module Main where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isPrefixOf)
import Data.Type.Equality ((:~:)(Refl))
import NativeNamedCallbackMeasuredZip (measuredZip, runMeasure)
import NativeNamedCallbackOtherSizes (otherSize)
import qualified PreludeMapLengthSize as A
import qualified PreludeMapLengthSize as B

check :: [Bool] -> [Int] -> Bool
check xs ys =
    case measuredZip A.size xs ys Refl of
        Refl -> case measuredZip (B.size @Bool) xs ys Refl of Refl -> True

main :: IO ()
main = do
    let element = error "element" :: Bool
        tailBottom = error "unused tail" :: [Bool]
        asInt A.Zero = 0 :: Int
        asInt (A.Succ n) = 1 + asInt n
    print (check [] (error "unused right"), check [element] [0])
    print (asInt (fst (runMeasure A.size [element, element])),
           asInt (fst (runMeasure otherSize [element, element])))
    print (case fst (runMeasure A.size (element : tailBottom)) of
             A.Zero -> False
             A.Succ _ -> True)
    result <- try @ErrorCall (evaluate (check [element] (error "required right")))
    case result of
        Left err | "required right" `isPrefixOf` displayException err -> pure ()
        _ -> fail "expected the right spine to be demanded"
