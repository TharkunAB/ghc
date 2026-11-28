{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
module NativeRevExplicitListO2RetainedOps (keepList, appRet, revRet) where

keepList :: foreach (xs :: [a]) -> [a]
keepList xs = xs

appRet :: [a] -> [a] -> [a]
appRet []       ys = ys
appRet (x : xs) ys = x : appRet xs ys

revRet :: [a] -> [a]
revRet []       = []
revRet (x : xs) = appRet (revRet xs) [x]
