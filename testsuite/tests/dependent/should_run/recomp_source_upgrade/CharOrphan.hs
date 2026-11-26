{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module Orphan () where
import CharacterCase (upper)
{-# RULES "change upper" forall c. upper c = 'Z' #-}
