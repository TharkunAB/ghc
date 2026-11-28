{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module NativeStructuralCountdownLib (Step(..), countdown, empty) where

import Data.Type.Equality ((:~:)(Refl))

data Step = Zero | Next Step

countdown :: Step -> [Step]
countdown Zero = []
countdown (Next rest) = rest : countdown rest

empty :: countdown 'Zero :~: ('[] :: [Step])
empty = Refl
