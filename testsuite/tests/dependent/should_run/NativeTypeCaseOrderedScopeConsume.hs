{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeCaseOrderedScopeConsume (render, different, renderPair, renderLazy, renderPartial, Lazy, Second) where
import NativeTypeCaseOrderedScopeDomain (Pair(..))

type Lazy (m :: Maybe Bool) = case m of { Nothing -> Char; Just _ -> Int }
type Second (p :: Pair) = case p of
    Pair _ False -> Int
    Pair False True -> Char
    Pair True True -> Bool

render :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Just False -> Bool; Nothing -> Char; Just True -> Int }) -> String
render _ v = case v of { Nothing -> "nothing"; Just _ -> "just" }

different :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Just False -> Bool; Nothing -> Char; Just True -> Ordering }) -> String
different _ _ = "different"

renderPair :: foreach (p :: Pair) ->
    Maybe (case p of { Pair _ False -> Int; Pair False True -> Char; Pair True True -> Bool }) -> String
renderPair _ _ = "pair"

renderLazy :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just _ -> Int }) -> String
renderLazy _ _ = "lazy"

renderPartial :: foreach (m :: Maybe Bool) ->
    Maybe (case m of { Nothing -> Char; Just True -> Int }) -> String
renderPartial _ _ = "partial"
