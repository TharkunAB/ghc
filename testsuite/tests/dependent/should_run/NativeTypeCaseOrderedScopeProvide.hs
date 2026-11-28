{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeCaseOrderedScopeProvide (pick, pickPair, pickEager, pickPartial, Eager, First) where
import NativeTypeCaseOrderedScopeDomain (Pair(..))

type Eager (m :: Maybe Bool) = case m of { Nothing -> Char; Just True -> Int; Just _ -> Int }
type First (p :: Pair) = case p of
    Pair False True -> Char
    Pair True True -> Bool
    Pair _ False -> Int

pick :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just True -> Int; Just False -> Bool })
pick _ = Nothing

pickPair :: foreach (p :: Pair) ->
    Maybe (case p of { Pair False True -> Char; Pair True True -> Bool; Pair _ False -> Int })
pickPair _ = Nothing

pickEager :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just True -> Int; Just _ -> Int })
pickEager _ = Nothing

pickPartial :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just True -> Int })
pickPartial _ = Nothing
