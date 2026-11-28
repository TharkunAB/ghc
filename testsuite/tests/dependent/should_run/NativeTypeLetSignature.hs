{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeLetSignature (capture, selected) where
import Data.Proxy (Proxy(..))

capture :: let { saved = a; a = Bool } in saved -> a -> saved
capture saved _ = saved

selected :: Proxy (let seed = 'True
                       first = case seed of { True -> Bool; False -> Char }
                       saved = first
                       result = case 'False of { True -> Int; False -> saved }
                   in result)
selected = Proxy @Bool
