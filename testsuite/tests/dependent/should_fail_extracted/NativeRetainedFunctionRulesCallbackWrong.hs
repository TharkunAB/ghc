{-# LANGUAGE NoDependentTypes #-}
module NativeRetainedFunctionRulesCallbackWrong where
import CallbackRules

newtype Wrapped = Wrapped Bool

checkNewtype :: (Wrapped -> Bool) -> Bool -> Int -> Int
checkNewtype f x value = readKept f x (makeChoice f x value)

checkWrong :: (Bool -> Bool) -> (Bool -> Bool) -> Bool -> Int -> Int
checkWrong f g x value = readKept f x (makeChoice g x value)
