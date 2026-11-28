{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeCaseRetainedThRenderAcross (render, renderMaybe, renderChanged) where
render :: foreach (b :: Bool) -> (case b of { False -> Char; True -> Int }) -> String
render True = show
render False = (:[])

renderMaybe :: foreach (m :: Maybe Bool) ->
    (case m of { Just _ -> Int; Nothing -> Char }) -> String
renderMaybe (Just _) = show
renderMaybe Nothing = (:[])

renderChanged :: foreach (m :: Maybe Bool) ->
    (case m of { Just _ -> Bool; Nothing -> Char }) -> String
renderChanged (Just _) = show
renderChanged Nothing = (:[])
