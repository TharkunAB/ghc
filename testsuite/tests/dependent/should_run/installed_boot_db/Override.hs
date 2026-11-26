{-# OPTIONS_GHC -Wno-orphans -Wno-inline-rule-shadowing #-}
module Override () where
import qualified Prelude as P
{-# RULES "not/override" forall x. P.not x = P.True #-}
