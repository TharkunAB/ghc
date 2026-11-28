{-# LANGUAGE DataKinds, DependentTypes, TemplateHaskell, TypeOperators #-}
module NativeTypeCaseQuote where

import Data.Type.Equality ((:~:)(Refl))

result :: $([t| case 'True of { True -> Int; False -> Bool } |]) :~: Int
result = Refl
