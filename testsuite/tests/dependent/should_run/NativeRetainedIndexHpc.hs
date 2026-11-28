{-# LANGUAGE DataKinds, NoDependentTypes #-}
module Main where

import Control.Monad (unless)
import NativeRetainedIndexHpcProvider
import Trace.Hpc.Mix (BoxLabel(..), Mix(..), readMix)
import Trace.Hpc.Reflect (examineTix)
import Trace.Hpc.Tix (Tix(..), TixModule(..))
import Trace.Hpc.Util (fromHpcPos)

data Inputs = Inputs (Tag ('S 'Z)) (Tag ('S 'Z))

readTag :: Tag n -> Int
readTag (Tag n) = n

choose :: Bool -> Inputs -> Int
choose allow (Inputs (Tag 1) second)
    | allow, Tag 2 <- second = 2
choose _ _ = -1

demandedIndex :: Tag ('S 'Z)
demandedIndex = deep (S Z)
{-# NOINLINE demandedIndex #-}

checkTick :: [TixModule] -> String -> (Int, Int, Int, Int) -> Integer -> IO ()
checkTick mods modName spanPos expected = do
    let matching = [ tm | tm@(TixModule name _ _ _) <- mods, name == modName ]
    case matching of
        [tm@(TixModule _ _ _ counts)] -> do
            Mix _ _ _ _ entries <- readMix [".hpc"] (Right tm)
            let hits = [ c | ((pos, ExpBox _), c) <- zip entries counts, fromHpcPos pos == spanPos ]
            unless (hits == [expected]) $
                fail ("Tick mismatch for " ++ modName ++ " " ++ show spanPos ++ ": " ++ show hits)
        _ -> fail ("Missing TixModule " ++ modName)

main :: IO ()
main = do
    print (readTag skippedIndex, readTag selectedIndex)
    print (choose False (Inputs selectedIndex guardedIndex))
    print (choose False (Inputs selectedIndex (error "unreached guard")))
    print (choose True (Inputs selectedIndex demandedIndex))
    Tix mods <- examineTix
    checkTick mods "NativeRetainedIndexHpcProvider" (20, 21, 20, 25) 0
    checkTick mods "NativeRetainedIndexHpcProvider" (20, 24, 20, 24) 0
    checkTick mods "NativeRetainedIndexHpcProvider" (24, 25, 24, 29) 1
    checkTick mods "NativeRetainedIndexHpcProvider" (24, 28, 24, 28) 0
    checkTick mods "NativeRetainedIndexHpcProvider" (28, 21, 28, 25) 0
    checkTick mods "NativeRetainedIndexHpcProvider" (28, 24, 28, 24) 0
    checkTick mods "Main" (22, 22, 22, 26) 1
    checkTick mods "Main" (22, 25, 22, 25) 1
