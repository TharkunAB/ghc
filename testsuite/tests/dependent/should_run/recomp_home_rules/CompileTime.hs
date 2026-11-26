{-# LANGUAGE TemplateHaskell #-}
module CompileTime where

import Language.Haskell.TH.Syntax (lift)
import Provider (opposite, lazyControls)

result :: ((Bool, Bool), (Bool, Bool), (Bool, ()))
result = $(lift (opposite True, opposite False, lazyControls))
