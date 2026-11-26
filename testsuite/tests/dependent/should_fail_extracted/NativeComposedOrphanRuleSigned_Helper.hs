{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeComposedOrphanRuleSigned_Helper () where
{-# RULES "nocode compose" forall f g x. (f . g) x = f x #-}
