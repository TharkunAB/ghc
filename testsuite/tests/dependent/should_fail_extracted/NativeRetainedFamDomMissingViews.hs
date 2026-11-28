{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoDependentTypes, QuantifiedConstraints, ScopedTypeVariables, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module NativeRetainedFamDomMissingViews where
import ConstrainedElements (Allowed, FieldViews(..), FormatKey, Formatted, RenderFormat(..))
import ConstrainedElementsClient (ViewsAfter)
import Data.Proxy (Proxy(..))
instance {-# OVERLAPPING #-} Allowed a => RenderFormat (FormatKey [a]) where
    type FieldView (FormatKey [a]) detailed = Formatted [a] detailed
renderRaw :: forall f a.
    ( forall x. Allowed x => RenderFormat (FormatKey (f x))
    , Show (f a)
    ) => Proxy f -> Proxy a -> ViewsAfter f a -> String
renderRaw _ _ (FieldViews raw _) = show raw
