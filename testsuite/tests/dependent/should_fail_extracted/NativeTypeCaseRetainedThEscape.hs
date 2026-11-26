{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedThEscape where
import Data.Proxy (Proxy)
import Language.Haskell.TH (Q, Type)

escape :: Q Type
escape = [t| foreach (b :: Maybe Bool) ->
             case b of { Just field -> Proxy field; Nothing -> Proxy field } |]
