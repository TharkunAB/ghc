{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedThSeparate where

choose :: $([t| foreach (b :: Bool) -> case b of { True -> Int; False -> Char } |])
choose True = 13
choose False = 'q'

$(pure [])

render :: $([t| foreach (b :: Bool) -> (case b of { False -> Char; True -> Int }) -> String |])
render True i = show i
render False c = show c

separate :: foreach (b :: Bool) -> String
separate b = render b (choose b)
