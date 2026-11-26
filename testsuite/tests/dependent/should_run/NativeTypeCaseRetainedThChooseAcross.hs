{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeCaseRetainedThChooseAcross (choose, chooseMaybe) where
choose :: foreach (b :: Bool) -> (case b of { True -> Int; False -> Char })
choose True = 19
choose False = 'q'

chooseMaybe :: foreach (m :: Maybe Bool) ->
    (case m of { Nothing -> Char; Just _ -> Int })
chooseMaybe Nothing = 'n'
chooseMaybe (Just _) = 73
