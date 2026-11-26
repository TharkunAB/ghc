{-# LANGUAGE DependentTypes, ListTuplePuns, LambdaCase #-}
module NativeTypeCaseScopedResultProvide (field, capture, nested) where
import Data.Proxy (Proxy(..))
import NativeTypeCaseScopedResultDomain (Choice(..))

field :: foreach (d :: Choice) ->
    (case d of { Neither -> Proxy 'False; Both l r -> Proxy l })
field Neither = Proxy
field (Both l _) = Proxy @l

capture :: foreach (a :: Bool) -> foreach (b :: Bool) -> foreach (d :: Choice) ->
    (case d of { Neither -> Proxy a; Both l r -> Proxy b })
capture a b = \cases
    Neither -> Proxy @a
    (Both _ _) -> Proxy @b

nested :: foreach (a :: Bool) -> foreach (d :: Choice) ->
    Maybe (case d of { Neither -> Proxy a; Both True r -> Proxy r; Both False r -> Proxy a })
nested _ _ = Nothing
