{-# LANGUAGE ExplicitForAll, NoDependentTypes #-}
{-# OPTIONS_GHC -fwrite-source-equations #-}
module NativeOpenZipRowsVariants
  ( zipConcrete, countRight, countLeft, countEither
  , loopLength, swapCycle, crossFields, growRight, growBoth ) where

import PackedVecLibrary (Nat(..))

zipConcrete :: [Bool] -> [Char] -> Nat
zipConcrete [] _ = Z
zipConcrete (_ : _) [] = Z
zipConcrete (_ : xs) (_ : ys) = S (zipConcrete xs ys)

countRight :: forall a b. [a] -> [b] -> Nat
countRight [] _ = Z
countRight (_ : _) [] = Z
countRight left@(_ : _) (_ : ys) = S (countRight left ys)

countLeft :: forall a b. [a] -> [b] -> Nat
countLeft [] _ = Z
countLeft (_ : _) [] = Z
countLeft (_ : xs) right@(_ : _) = S (countLeft xs right)

countEither :: [Bool] -> [Bool] -> Nat
countEither [] _ = Z
countEither (_ : _) [] = Z
countEither (_ : xs) right@(False : _) = S (countEither xs right)
countEither left@(_ : _) (True : ys) = S (countEither left ys)

loopLength :: [Bool] -> [Char] -> Nat
loopLength [] _ = Z
loopLength (_ : _) [] = Z
loopLength left@(_ : _) right@(_ : _) = S (loopLength left right)

swapCycle :: [Bool] -> [Bool] -> Nat
swapCycle [] _ = Z
swapCycle (_ : _) [] = Z
swapCycle left@(_ : _) right@(_ : _) = S (swapCycle right left)

crossFields :: [Bool] -> [Bool] -> Nat
crossFields [] _ = Z
crossFields (_ : _) [] = Z
crossFields (_ : xs) (False : ys) = S (crossFields xs ys)
crossFields (_ : xs) (True : ys) = S (crossFields ys xs)

growRight :: [Bool] -> [Char] -> Nat
growRight [] _ = Z
growRight (_ : _) [] = Z
growRight (_ : xs) (y : ys) = S (growRight xs (y : y : ys))

growBoth :: [Bool] -> [Char] -> Nat
growBoth [] _ = Z
growBoth (_ : _) [] = Z
growBoth (x : xs) (y : ys) = S (growBoth (x : x : xs) (y : y : ys))
