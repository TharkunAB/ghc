{-# LANGUAGE DataKinds, PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module NativeSameModOrphanBootRule_Orphan () where
import {-# SOURCE #-} NativeSameModOrphanBootRule (Ty(..), Ctx(..), Term, substSingle)
{-# RULES "orphan/substSingle" [~1] forall (m :: Term ('CExt g 'TNat) 'TNat) (v :: Term g 'TNat). substSingle m v = v #-}
