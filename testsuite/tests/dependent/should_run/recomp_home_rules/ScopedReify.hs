{-# LANGUAGE TemplateHaskell #-}
module ScopedReify where
import Language.Haskell.TH.Syntax (reifyType)
import Provider (chooseTagged)
$(reifyType 'chooseTagged >> pure [])
