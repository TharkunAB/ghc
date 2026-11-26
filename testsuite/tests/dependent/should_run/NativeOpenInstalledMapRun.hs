{-# LANGUAGE GADTs, NoDependentTypes #-}
module Main where
import Data.IORef (newIORef, readIORef)
import Data.Type.Equality ((:~:)(Refl))
import NativeRetainedSourceOperand (ordinaryNot, select)
import PreludeFilterAppendProof (showPreludeFilterAppend)
import PreludeFilterAppendRuntime (RuntimeFilterAppend(..), packFilterAppend)
import PreludeFilterLengthProof (showPreludeFilterLength)
import PreludeFilterProof (lazyHead, lazyIdentityHead)
import PreludeMapLengthProof (showPreludeMapLength)

observe :: RuntimeFilterAppend -> ([Bool], [Bool])
observe (RuntimeFilterAppend xs ys Refl) =
    (filter not (xs ++ ys), filter not xs ++ filter not ys)

main :: IO ()
main = do
    putStrLn showPreludeMapLength
    let bottom = error "filter forced the tail"
    print (lazyHead bottom, lazyIdentityHead bottom)
    putStrLn showPreludeFilterLength
    putStrLn showPreludeFilterAppend
    xs <- newIORef [True, False] >>= readIORef
    ys <- newIORef [False, True] >>= readIORef
    print (observe (packFilterAppend xs ys), ordinaryNot True, select True False)
