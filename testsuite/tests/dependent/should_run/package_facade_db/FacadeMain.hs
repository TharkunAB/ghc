{-# LANGUAGE NoDependentTypes #-}
module Main where
import PublicVector (CharVector, fromChar, storedLength, storedCells, natToInt, toList)

readCells :: CharVector -> String
readCells p = toList (storedCells p)

main :: IO ()
main =
    let p = fromChar 'r'
    in print (natToInt (storedLength p), readCells p)
