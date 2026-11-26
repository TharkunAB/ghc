{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedThSeparateWrong where

choose :: $([t| foreach (b :: Bool) -> case b of { True -> Int; False -> Char } |])
choose True = 13
choose False = 'q'

renderSame :: $([t| foreach (b :: Bool) -> (case b of { True -> Int; False -> Char }) -> String |])
renderSame True i = show i
renderSame False c = show c

renderDiffArms :: $([t| foreach (b :: Bool) -> (case b of { True -> Int; False -> Bool }) -> String |])
renderDiffArms True i = show i
renderDiffArms False c = show c

diffArms :: foreach (b :: Bool) -> String
diffArms b = renderDiffArms b (choose b)

diffCapture :: foreach (b :: Bool) -> foreach (c :: Bool) -> String
diffCapture b c = renderSame c (choose b)
