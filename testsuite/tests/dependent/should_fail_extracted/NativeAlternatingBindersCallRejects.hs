{-# LANGUAGE DataKinds, DependentTypes, ExplicitNamespaces, ListTuplePuns, Modifiers #-}
{-# LANGUAGE PolyKinds, RequiredTypeArguments, ScopedTypeVariables, TypeAbstractions #-}
{-# LANGUAGE TypeApplications, TypeOperators #-}
module NativeAlternatingBindersCallRejects where

import Data.Type.Equality ((:~:))
import GHC.Exts (M)
import NativeAlternatingBindersProvider

wrongKind = forwardKind True @'EQ Marker
wrongReqKind = requiredKind True (type 'EQ) Marker

wrongIndex :: foreach (left :: Bool) -> foreach (right :: Bool) ->
  forall a. foreach (m :: Maybe a) -> Reading left m -> Reading right m
wrongIndex _ r @a m x = keepAfter r m x

wrongProof :: foreach (phase :: Phase) -> forall a.
  foreach (xs :: [Maybe a]) -> foreach (ys :: [Maybe a]) ->
  ('Nothing ': ys) :~: ('Nothing ': ys) ->
  ConsReading phase ('Nothing ': xs) -> ConsReading phase ('Nothing ': xs)
wrongProof phase @a xs _ p x = keepCons phase @a xs p x

wrongPhase :: foreach (phase :: Phase) -> foreach (other :: Phase) -> forall a.
  foreach (xs :: [Maybe a]) -> ('Nothing ': xs) :~: ('Nothing ': xs) ->
  ConsReading phase ('Nothing ': xs) -> ConsReading phase ('Nothing ': xs)
wrongPhase _ other @a xs p x = keepCons other @a xs p x
