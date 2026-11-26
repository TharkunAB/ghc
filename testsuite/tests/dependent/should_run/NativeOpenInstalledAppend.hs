{-# LANGUAGE NoDependentTypes #-}
module Main where

import PreludeAppendLengthProof (showPreludeAppendLength)
import PreludeMapAppendProof (showPreludeMapAppend)
import PreludeMapCompositionProof (showPreludeMapComposition)

main :: IO ()
main = do
  putStrLn showPreludeAppendLength
  putStrLn showPreludeMapAppend
  putStrLn showPreludeMapComposition
