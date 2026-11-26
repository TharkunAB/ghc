{-# LANGUAGE DependentTypes, ListTuplePuns, LambdaCase #-}
module NativeTypeCaseScopedResultConsume (sameField, otherField, sameCapture, otherCapture, nested, nestedOther) where
import Data.Proxy (Proxy(..))
import NativeTypeCaseScopedResultDomain (Choice(..))

sameField :: foreach (x :: Choice) ->
    (case x of { Both first second -> Proxy first; Neither -> Proxy 'False }) -> String
sameField Neither Proxy = "neither"
sameField (Both _ _) Proxy = "both"

otherField :: foreach (x :: Choice) ->
    (case x of { Both first second -> Proxy second; Neither -> Proxy 'False }) -> String
otherField _ _ = "wrong field"

sameCapture :: foreach (x :: Bool) -> foreach (y :: Bool) -> foreach (d :: Choice) ->
    (case d of { Both l r -> Proxy y; Neither -> Proxy x }) -> String
sameCapture _ _ = \cases
    Neither Proxy -> "neither"
    (Both _ _) Proxy -> "both"

otherCapture :: foreach (x :: Bool) -> foreach (y :: Bool) -> foreach (d :: Choice) ->
    (case d of { Both l r -> Proxy x; Neither -> Proxy y }) -> String
otherCapture _ _ _ _ = "wrong capture"

nested :: foreach (a :: Bool) -> foreach (d :: Choice) ->
    Maybe (case d of { Both False t -> Proxy a; Neither -> Proxy a; Both True t -> Proxy t }) -> String
nested _ _ _ = "nested"

nestedOther :: foreach (a :: Bool) -> foreach (d :: Choice) ->
    Maybe (case d of { Both False t -> Proxy t; Neither -> Proxy a; Both True t -> Proxy a }) -> String
nestedOther _ _ _ = "wrong nested result"
